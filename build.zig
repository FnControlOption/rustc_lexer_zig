// Based on https://github.com/andrewrk/StaticHttpFileServer/blob/main/build.zig

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const module = b.addModule("rustc_lexer", .{
        .root_source_file = b.path("rustc_lexer.zig"),
        .target = target,
        .optimize = optimize,
    });

    module.addImport("unicode_properties", b.dependency("unicode_properties", .{
        .target = target,
        .optimize = optimize,
    }).module("unicode_properties"));

    module.addImport("unicode_xid", b.dependency("unicode_xid", .{
        .target = target,
        .optimize = optimize,
    }).module("unicode_xid"));

    const unit_tests = b.addTest(.{
        .root_source_file = b.path("tests.zig"),
        .target = target,
        .optimize = optimize,
    });
    unit_tests.root_module.addImport("rustc_lexer", module);
    const run_unit_tests = b.addRunArtifact(unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}
