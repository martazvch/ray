const std = @import("std");
pub fn main(init: std.process.Init) !void {
    _ = init; // autofix
    const value = try std.fmt.parseInt(i64, "~4", 10);
    std.log.debug("Value: {}", .{value});
}
