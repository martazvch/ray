const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const oom = @import("utils.zig").oom;

fn Range(T: type) type {
    return struct {
        low: T,
        high: T,

        const Self = @This();
        pub const empty: Self = .{ .low = 0, .high = 0 };

        pub fn unit(value: T) Self {
            return .{ .low = value, .high = value };
        }

        pub fn isIncreasing(self: Self) bool {
            return self.low <= self.high;
        }
    };
}

pub fn RangeSet(T: type) type {
    if (T != i64 and T != f64) {
        @compileError("RangeSet can only accept i64 or f64 types");
    }

    return struct {
        ranges: ArrayList(InnerRange),

        const Self = @This();
        pub const InnerRange = Range(T);
        pub const Error = error{ partial, unreached };

        pub const empty: Self = .{ .ranges = .empty };

        pub fn deinit(self: *Self, allocator: Allocator) void {
            self.ranges.deinit(allocator);
        }

        /// Computes uncovered parts of a range as a sorted list
        /// Given a range, return all parts of r not yet covered
        pub fn subtract(self: *Self, range: InnerRange, allocator: Allocator) ArrayList(InnerRange) {
            // Uncovered fragments of range
            var out: ArrayList(InnerRange) = .empty;

            // We start with the entire range [cur_low, cur_high]
            // range:  [=========================]
            //          ^cur_low                ^cur_high
            var cur_low = range.low;
            const cur_high = range.high;

            // self.ranges is sorted and non-overlapping
            for (self.ranges.items) |c| {
                // Skip ranges that end before our current window
                //  cur:    [2---4]
                //  range:          [6-----------]
                if (c.high < cur_low) continue;

                // Stop if covered range starts after our window
                //  cur:                    [10---12]
                //  range:  [3-----------8]
                if (c.low > cur_high) break;

                // From here, covered range overlaps our window

                // Case 1 — uncovered gap before cur
                //  range:    [4---6]
                //  cur: [1-------------10]
                //  uncovered:  [1---3]
                if (c.low > cur_low) {
                    out.append(allocator, .{ .low = cur_low, .high = c.low - 1 }) catch oom();
                }

                // Case 2 — shrink the window past the covered range
                //  range:    [4---6]
                //  cur_low:         ^
                // Values ≤ cur.high are now known to be covered
                cur_low = c.high + 1;

                // Stop if nothighng left
                if (cur_low > cur_high) break;
            }

            // After the lowop: tail uncovered segment
            //  covered: [2---4]
            //  range:        [1-----------8]
            //  remaining: [5---8]
            if (cur_low <= cur_high) {
                out.append(allocator, .{ .low = cur_low, .high = cur_high }) catch oom();
            }

            return out;
        }

        /// Merge a new range into covered
        /// Insert range into covered whighle preserving invariants
        pub fn add(self: *Self, allocator: Allocator, range: InnerRange) void {
            var low = range.low;
            var high = range.high;

            var i: usize = 0;
            while (i < self.ranges.items.len) {
                const cur = self.ranges.items[i];

                // Case 1 — new range is strictly before current
                //  cur:          [5---7]
                //  new: [1---3]
                if (high + 1 < cur.low) break;

                // Case 2 — new range is strictly after current
                //  cur: [1---3]
                //  new:          [5---7]
                if (low > cur.high + 1) {
                    i += 1;
                    continue;
                }

                // Overlap or adjacency → merge
                //  cur: [3---5]
                //  new:     [5---8]
                //  merged: [3-------8]
                low = @min(low, cur.low);
                high = @max(high, cur.high);
                _ = self.ranges.orderedRemove(i);
            }

            self.ranges.insert(allocator, i, .{ .low = low, .high = high }) catch oom();
        }

        pub fn addAll(self: *Self, allocator: Allocator, ranges: []const InnerRange) void {
            for (ranges) |range| {
                self.add(allocator, range);
            }
        }

        pub fn checkRange(self: *Self, allocator: Allocator, range: InnerRange) Error!void {
            var uncovered = self.subtract(range, allocator);
            defer uncovered.deinit(allocator);

            if (uncovered.items.len == 0) {
                return error.unreached;
            }

            if (uncovered.items.len != 1 or
                uncovered.items[0].low != range.low or
                uncovered.items[0].high != range.high)
            {
                return error.partial;
            }

            self.addAll(allocator, uncovered.items);
        }
    };
}

// pub const Range = struct {
//     low: i64,
//     high: i64,
//
//     pub const empty: Range = .{ .low = 0, .high = 0 };
//
//     pub fn unit(value: i64) Range {
//         return .{ .low = value, .high = value };
//     }
//
//     pub fn isIncreasing(self: Range) bool {
//         return self.low <= self.high;
//     }
// };

test "single scalar int" {
    const ranges: []const Range(i64) = &.{
        .empty,
    };
    const covered = runTestCheck(ranges) catch return error.TestFailure;
    defer std.testing.allocator.free(covered);

    try std.testing.expect(covered.len == 1);
    try std.testing.expectEqualDeep(covered[0], Range(i64).empty);
}

test "continous scalar int" {
    const ranges: []const Range(i64) = &.{
        .{ .low = 0, .high = 0 }, .{ .low = 1, .high = 1 },
    };
    const covered = runTestCheck(ranges) catch return error.TestFailure;
    defer std.testing.allocator.free(covered);

    try std.testing.expect(covered.len == 1);
    try std.testing.expectEqualDeep(covered[0], Range(i64){ .low = 0, .high = 1 });
}

test "discontinous scalar int" {
    const ranges: []const Range(i64) = &.{
        .{ .low = 0, .high = 0 }, .{ .low = 3, .high = 3 },
    };
    const covered = runTestCheck(ranges) catch return error.TestFailure;
    defer std.testing.allocator.free(covered);

    try std.testing.expect(covered.len == 2);
    try std.testing.expectEqualDeep(covered[0], Range(i64){ .low = 0, .high = 0 });
    try std.testing.expectEqualDeep(covered[1], Range(i64){ .low = 3, .high = 3 });
}

test "single range int" {
    const ranges: []const Range(i64) = &.{
        .{ .low = 2, .high = 4 },
    };
    const covered = runTestCheck(ranges) catch return error.TestFailure;
    defer std.testing.allocator.free(covered);

    try std.testing.expect(covered.len == 1);
    try std.testing.expectEqualDeep(covered[0], Range(i64){ .low = 2, .high = 4 });
}

test "continous range" {
    const ranges: []const Range(i64) = &.{
        .{ .low = 0, .high = 4 }, .{ .low = 5, .high = 8 },
    };
    const covered = runTestCheck(ranges) catch return error.TestFailure;
    defer std.testing.allocator.free(covered);

    try std.testing.expect(covered.len == 1);
    try std.testing.expectEqualDeep(covered[0], Range(i64){ .low = 0, .high = 8 });
}

test "discontinous ranges" {
    const ranges: []const Range(i64) = &.{
        .{ .low = 3, .high = 6 }, .{ .low = 8, .high = 12 },
    };
    const covered = runTestCheck(ranges) catch return error.TestFailure;
    defer std.testing.allocator.free(covered);

    try std.testing.expect(covered.len == 2);
    try std.testing.expectEqualDeep(covered[0], Range(i64){ .low = 3, .high = 6 });
    try std.testing.expectEqualDeep(covered[1], Range(i64){ .low = 8, .high = 12 });
}

test "errors" {
    var ranges: []const Range(i64) = &.{
        .{ .low = 3, .high = 6 }, .{ .low = 5, .high = 8 },
    };
    _ = runTestCheck(ranges) catch |err| {
        try std.testing.expect(err == RangeSet(i64).Error.partial);
    };

    ranges = &.{
        .{ .low = 3, .high = 6 }, .{ .low = 7, .high = 9 }, .{ .low = 5, .high = 6 },
    };
    _ = runTestCheck(ranges) catch |err| {
        try std.testing.expect(err == RangeSet(i64).Error.unreached);
    };
}

fn runTestCheck(ranges: []const Range(i64)) RangeSet(i64).Error![]const Range(i64) {
    const allocator = std.testing.allocator;
    var covered: RangeSet(i64) = .empty;
    defer covered.deinit(allocator);

    for (ranges) |range| {
        try covered.checkRange(allocator, range);
    }

    return covered.ranges.toOwnedSlice(allocator) catch oom();
}
