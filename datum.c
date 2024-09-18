/*
 * Copyright (c) 2016 Laurent Pinchart <laurent.pinchart@ideasonboard.com>
 *
 * DRM core format related functions
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that copyright
 * notice and this permission notice appear in supporting documentation, and
 * that the name of the copyright holders not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  The copyright holders make no representations
 * about the suitability of this software for any purpose.  It is provided "as
 * is" without express or implied warranty.
 *
 * THE COPYRIGHT HOLDERS DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
 * OF THIS SOFTWARE.
 */

#include <inttypes.h>
#include <string.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <drm/drm_fourcc.h>
#include <assert.h>
#include "drm_fourcc.h"
#define ARRAY_SIZE(x) (sizeof(x)/sizeof((x)[0]))
static const struct drm_format_info formats[] = {
  { .format = DRM_FORMAT_C1,		.depth = 1,  .num_planes = 1,
    .char_per_block = { 1, }, .block_w = { 8, }, .block_h = { 1, }, .hsub = 1, .vsub = 1, .is_color_indexed = true },
  { .format = DRM_FORMAT_C2,		.depth = 2,  .num_planes = 1,
    .char_per_block = { 1, }, .block_w = { 4, }, .block_h = { 1, }, .hsub = 1, .vsub = 1, .is_color_indexed = true },
  { .format = DRM_FORMAT_C4,		.depth = 4,  .num_planes = 1,
    .char_per_block = { 1, }, .block_w = { 2, }, .block_h = { 1, }, .hsub = 1, .vsub = 1, .is_color_indexed = true },
  { .format = DRM_FORMAT_C8,		.depth = 8,  .num_planes = 1, .cpp = { 1, 0, 0 }, .hsub = 1, .vsub = 1, .is_color_indexed = true },
  { .format = DRM_FORMAT_D1,		.depth = 1,  .num_planes = 1,
    .char_per_block = { 1, }, .block_w = { 8, }, .block_h = { 1, }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_D2,		.depth = 2,  .num_planes = 1,
    .char_per_block = { 1, }, .block_w = { 4, }, .block_h = { 1, }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_D4,		.depth = 4,  .num_planes = 1,
    .char_per_block = { 1, }, .block_w = { 2, }, .block_h = { 1, }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_D8,		.depth = 8,  .num_planes = 1, .cpp = { 1, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_R1,		.depth = 1,  .num_planes = 1,
    .char_per_block = { 1, }, .block_w = { 8, }, .block_h = { 1, }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_R2,		.depth = 2,  .num_planes = 1,
    .char_per_block = { 1, }, .block_w = { 4, }, .block_h = { 1, }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_R4,		.depth = 4,  .num_planes = 1,
    .char_per_block = { 1, }, .block_w = { 2, }, .block_h = { 1, }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_R8,		.depth = 8,  .num_planes = 1, .cpp = { 1, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_R10,		.depth = 10, .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_R12,		.depth = 12, .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_RGB332,		.depth = 8,  .num_planes = 1, .cpp = { 1, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_BGR233,		.depth = 8,  .num_planes = 1, .cpp = { 1, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_XRGB4444,	.depth = 0,  .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_XBGR4444,	.depth = 0,  .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_RGBX4444,	.depth = 0,  .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_BGRX4444,	.depth = 0,  .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_ARGB4444,	.depth = 0,  .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_ABGR4444,	.depth = 0,  .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_RGBA4444,	.depth = 0,  .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_BGRA4444,	.depth = 0,  .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_XRGB1555,	.depth = 15, .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_XBGR1555,	.depth = 15, .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_RGBX5551,	.depth = 15, .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_BGRX5551,	.depth = 15, .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_ARGB1555,	.depth = 15, .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_ABGR1555,	.depth = 15, .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_RGBA5551,	.depth = 15, .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_BGRA5551,	.depth = 15, .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_RGB565,		.depth = 16, .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_BGR565,		.depth = 16, .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 1, .vsub = 1 },
#if __BYTE_ORDER__ == __ORDER_PDP_ENDIAN__
# error PDP endian not supported
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  { .format = DRM_FORMAT_XRGB1555 | DRM_FORMAT_BIG_ENDIAN, .depth = 15, .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_RGB565 | DRM_FORMAT_BIG_ENDIAN, .depth = 16, .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 1, .vsub = 1 },
#elif __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#else
# error compiler bug
#endif
  { .format = DRM_FORMAT_RGB888,		.depth = 24, .num_planes = 1, .cpp = { 3, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_BGR888,		.depth = 24, .num_planes = 1, .cpp = { 3, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_XRGB8888,	.depth = 24, .num_planes = 1, .cpp = { 4, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_XBGR8888,	.depth = 24, .num_planes = 1, .cpp = { 4, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_RGBX8888,	.depth = 24, .num_planes = 1, .cpp = { 4, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_BGRX8888,	.depth = 24, .num_planes = 1, .cpp = { 4, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_RGB565_A8,	.depth = 24, .num_planes = 2, .cpp = { 2, 1, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_BGR565_A8,	.depth = 24, .num_planes = 2, .cpp = { 2, 1, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_XRGB2101010,	.depth = 30, .num_planes = 1, .cpp = { 4, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_XBGR2101010,	.depth = 30, .num_planes = 1, .cpp = { 4, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_RGBX1010102,	.depth = 30, .num_planes = 1, .cpp = { 4, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_BGRX1010102,	.depth = 30, .num_planes = 1, .cpp = { 4, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_ARGB2101010,	.depth = 30, .num_planes = 1, .cpp = { 4, 0, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_ABGR2101010,	.depth = 30, .num_planes = 1, .cpp = { 4, 0, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_RGBA1010102,	.depth = 30, .num_planes = 1, .cpp = { 4, 0, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_BGRA1010102,	.depth = 30, .num_planes = 1, .cpp = { 4, 0, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_ARGB8888,	.depth = 32, .num_planes = 1, .cpp = { 4, 0, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_ABGR8888,	.depth = 32, .num_planes = 1, .cpp = { 4, 0, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_RGBA8888,	.depth = 32, .num_planes = 1, .cpp = { 4, 0, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_BGRA8888,	.depth = 32, .num_planes = 1, .cpp = { 4, 0, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_XRGB16161616F,	.depth = 0,  .num_planes = 1, .cpp = { 8, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_XBGR16161616F,	.depth = 0,  .num_planes = 1, .cpp = { 8, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_ARGB16161616F,	.depth = 0,  .num_planes = 1, .cpp = { 8, 0, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_ABGR16161616F,	.depth = 0,  .num_planes = 1, .cpp = { 8, 0, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_AXBXGXRX106106106106, .depth = 0, .num_planes = 1, .cpp = { 8, 0, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_XRGB16161616,	.depth = 0,  .num_planes = 1, .cpp = { 8, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_XBGR16161616,	.depth = 0,  .num_planes = 1, .cpp = { 8, 0, 0 }, .hsub = 1, .vsub = 1 },
  { .format = DRM_FORMAT_ARGB16161616,	.depth = 0,  .num_planes = 1, .cpp = { 8, 0, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_ABGR16161616,	.depth = 0,  .num_planes = 1, .cpp = { 8, 0, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_RGB888_A8,	.depth = 32, .num_planes = 2, .cpp = { 3, 1, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_BGR888_A8,	.depth = 32, .num_planes = 2, .cpp = { 3, 1, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_XRGB8888_A8,	.depth = 32, .num_planes = 2, .cpp = { 4, 1, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_XBGR8888_A8,	.depth = 32, .num_planes = 2, .cpp = { 4, 1, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_RGBX8888_A8,	.depth = 32, .num_planes = 2, .cpp = { 4, 1, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_BGRX8888_A8,	.depth = 32, .num_planes = 2, .cpp = { 4, 1, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true },
  { .format = DRM_FORMAT_YUV410,		.depth = 0,  .num_planes = 3, .cpp = { 1, 1, 1 }, .hsub = 4, .vsub = 4, .is_yuv = true },
  { .format = DRM_FORMAT_YVU410,		.depth = 0,  .num_planes = 3, .cpp = { 1, 1, 1 }, .hsub = 4, .vsub = 4, .is_yuv = true },
  { .format = DRM_FORMAT_YUV411,		.depth = 0,  .num_planes = 3, .cpp = { 1, 1, 1 }, .hsub = 4, .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_YVU411,		.depth = 0,  .num_planes = 3, .cpp = { 1, 1, 1 }, .hsub = 4, .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_YUV420,		.depth = 0,  .num_planes = 3, .cpp = { 1, 1, 1 }, .hsub = 2, .vsub = 2, .is_yuv = true },
  { .format = DRM_FORMAT_YVU420,		.depth = 0,  .num_planes = 3, .cpp = { 1, 1, 1 }, .hsub = 2, .vsub = 2, .is_yuv = true },
  { .format = DRM_FORMAT_YUV422,		.depth = 0,  .num_planes = 3, .cpp = { 1, 1, 1 }, .hsub = 2, .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_YVU422,		.depth = 0,  .num_planes = 3, .cpp = { 1, 1, 1 }, .hsub = 2, .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_YUV444,		.depth = 0,  .num_planes = 3, .cpp = { 1, 1, 1 }, .hsub = 1, .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_YVU444,		.depth = 0,  .num_planes = 3, .cpp = { 1, 1, 1 }, .hsub = 1, .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_NV12,		.depth = 0,  .num_planes = 2, .cpp = { 1, 2, 0 }, .hsub = 2, .vsub = 2, .is_yuv = true },
  { .format = DRM_FORMAT_NV21,		.depth = 0,  .num_planes = 2, .cpp = { 1, 2, 0 }, .hsub = 2, .vsub = 2, .is_yuv = true },
  { .format = DRM_FORMAT_NV16,		.depth = 0,  .num_planes = 2, .cpp = { 1, 2, 0 }, .hsub = 2, .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_NV61,		.depth = 0,  .num_planes = 2, .cpp = { 1, 2, 0 }, .hsub = 2, .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_NV24,		.depth = 0,  .num_planes = 2, .cpp = { 1, 2, 0 }, .hsub = 1, .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_NV42,		.depth = 0,  .num_planes = 2, .cpp = { 1, 2, 0 }, .hsub = 1, .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_YUYV,		.depth = 0,  .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 2, .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_YVYU,		.depth = 0,  .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 2, .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_UYVY,		.depth = 0,  .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 2, .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_VYUY,		.depth = 0,  .num_planes = 1, .cpp = { 2, 0, 0 }, .hsub = 2, .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_XYUV8888,	.depth = 0,  .num_planes = 1, .cpp = { 4, 0, 0 }, .hsub = 1, .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_VUY888,          .depth = 0,  .num_planes = 1, .cpp = { 3, 0, 0 }, .hsub = 1, .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_AYUV,		.depth = 0,  .num_planes = 1, .cpp = { 4, 0, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true, .is_yuv = true },
  { .format = DRM_FORMAT_Y210,            .depth = 0,  .num_planes = 1, .cpp = { 4, 0, 0 }, .hsub = 2, .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_Y212,            .depth = 0,  .num_planes = 1, .cpp = { 4, 0, 0 }, .hsub = 2, .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_Y216,            .depth = 0,  .num_planes = 1, .cpp = { 4, 0, 0 }, .hsub = 2, .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_Y410,            .depth = 0,  .num_planes = 1, .cpp = { 4, 0, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true, .is_yuv = true },
  { .format = DRM_FORMAT_Y412,            .depth = 0,  .num_planes = 1, .cpp = { 8, 0, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true, .is_yuv = true },
  { .format = DRM_FORMAT_Y416,            .depth = 0,  .num_planes = 1, .cpp = { 8, 0, 0 }, .hsub = 1, .vsub = 1, .has_alpha = true, .is_yuv = true },
  { .format = DRM_FORMAT_XVYU2101010,	.depth = 0,  .num_planes = 1, .cpp = { 4, 0, 0 }, .hsub = 1, .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_XVYU12_16161616,	.depth = 0,  .num_planes = 1, .cpp = { 8, 0, 0 }, .hsub = 1, .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_XVYU16161616,	.depth = 0,  .num_planes = 1, .cpp = { 8, 0, 0 }, .hsub = 1, .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_Y0L0,		.depth = 0,  .num_planes = 1,
    .char_per_block = { 8, 0, 0 }, .block_w = { 2, 0, 0 }, .block_h = { 2, 0, 0 },
    .hsub = 2, .vsub = 2, .has_alpha = true, .is_yuv = true },
  { .format = DRM_FORMAT_X0L0,		.depth = 0,  .num_planes = 1,
    .char_per_block = { 8, 0, 0 }, .block_w = { 2, 0, 0 }, .block_h = { 2, 0, 0 },
    .hsub = 2, .vsub = 2, .is_yuv = true },
  { .format = DRM_FORMAT_Y0L2,		.depth = 0,  .num_planes = 1,
    .char_per_block = { 8, 0, 0 }, .block_w = { 2, 0, 0 }, .block_h = { 2, 0, 0 },
    .hsub = 2, .vsub = 2, .has_alpha = true, .is_yuv = true },
  { .format = DRM_FORMAT_X0L2,		.depth = 0,  .num_planes = 1,
    .char_per_block = { 8, 0, 0 }, .block_w = { 2, 0, 0 }, .block_h = { 2, 0, 0 },
    .hsub = 2, .vsub = 2, .is_yuv = true },
  { .format = DRM_FORMAT_P010,            .depth = 0,  .num_planes = 2,
    .char_per_block = { 2, 4, 0 }, .block_w = { 1, 1, 0 }, .block_h = { 1, 1, 0 },
    .hsub = 2, .vsub = 2, .is_yuv = true},
  { .format = DRM_FORMAT_P012,		.depth = 0,  .num_planes = 2,
    .char_per_block = { 2, 4, 0 }, .block_w = { 1, 1, 0 }, .block_h = { 1, 1, 0 },
    .hsub = 2, .vsub = 2, .is_yuv = true},
  { .format = DRM_FORMAT_P016,		.depth = 0,  .num_planes = 2,
    .char_per_block = { 2, 4, 0 }, .block_w = { 1, 1, 0 }, .block_h = { 1, 1, 0 },
    .hsub = 2, .vsub = 2, .is_yuv = true},
  { .format = DRM_FORMAT_P210,		.depth = 0,
    .num_planes = 2, .char_per_block = { 2, 4, 0 },
    .block_w = { 1, 1, 0 }, .block_h = { 1, 1, 0 }, .hsub = 2,
    .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_VUY101010,	.depth = 0,
    .num_planes = 1, .cpp = { 0, 0, 0 }, .hsub = 1, .vsub = 1,
    .is_yuv = true },
  { .format = DRM_FORMAT_YUV420_8BIT,     .depth = 0,
    .num_planes = 1, .cpp = { 0, 0, 0 }, .hsub = 2, .vsub = 2,
    .is_yuv = true },
  { .format = DRM_FORMAT_YUV420_10BIT,    .depth = 0,
    .num_planes = 1, .cpp = { 0, 0, 0 }, .hsub = 2, .vsub = 2,
    .is_yuv = true },
  { .format = DRM_FORMAT_NV15,		.depth = 0,
    .num_planes = 2, .char_per_block = { 5, 5, 0 },
    .block_w = { 4, 2, 0 }, .block_h = { 1, 1, 0 }, .hsub = 2,
    .vsub = 2, .is_yuv = true },
  { .format = DRM_FORMAT_NV20,		.depth = 0,
    .num_planes = 2, .char_per_block = { 5, 5, 0 },
    .block_w = { 4, 2, 0 }, .block_h = { 1, 1, 0 }, .hsub = 2,
    .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_NV30,		.depth = 0,
    .num_planes = 2, .char_per_block = { 5, 5, 0 },
    .block_w = { 4, 2, 0 }, .block_h = { 1, 1, 0 }, .hsub = 1,
    .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_Q410,		.depth = 0,
    .num_planes = 3, .char_per_block = { 2, 2, 2 },
    .block_w = { 1, 1, 1 }, .block_h = { 1, 1, 1 }, .hsub = 1,
    .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_Q401,		.depth = 0,
    .num_planes = 3, .char_per_block = { 2, 2, 2 },
    .block_w = { 1, 1, 1 }, .block_h = { 1, 1, 1 }, .hsub = 1,
    .vsub = 1, .is_yuv = true },
  { .format = DRM_FORMAT_P030,            .depth = 0,  .num_planes = 2,
    .char_per_block = { 4, 8, 0 }, .block_w = { 3, 3, 0 }, .block_h = { 1, 1, 0 },
    .hsub = 2, .vsub = 2, .is_yuv = true},
};

static_assert(DRM_FORMAT_P030 != DRM_FORMAT_C1, "bug");
static_assert(ARRAY_SIZE(formats) == 118, "bug");
const char *lookup(uint32_t format)
{
  switch (format) {
#include "case.c"
  default:
    fprintf(stderr, "BAD VALUE %" PRIu32 "\n", format);
    abort();
  }
}

#pragma GCC diagnostic error "-Wformat"

static void print_buffer(const uint8_t buffer[static 4], uint8_t num_planes, const char *name)
{
  uint8_t buf[4];
  for (int i = 0; i < 4; ++i) {
    if (i >= num_planes) {
      if (buffer[i] != 0)
	abort();
      buf[i] = 0;
    } else {
      buf[i] = buffer[i] > 0 ? buffer[i] : 1;
    }
  }
  printf("  , .%-*s = { %" PRIu8 ", %" PRIu8 ", %" PRIu8 ", %" PRIu8 " }\n",
	 (int)strlen("is_color_indexed"), name, buf[0], buf[1], buf[2], buf[3]);
}

int main(void) {
  printf("#include <stdbool.h>\n"
	 "#include <drm/drm_fourcc.h>\n"
	 "#include \"drm_fourcc.h\"\n"
	 "\n");
  printf("struct drm_format_info formats[%zu] = {\n", ARRAY_SIZE(formats));
  for (size_t z = 0; z < ARRAY_SIZE(formats); ++z) {
    const size_t i = z;
    const uint32_t format_number = formats[i].format;
    const char *const name = lookup(format_number);
    const uint8_t num_planes = formats[i].num_planes;
    {
      const uint32_t lowered_format = format_number & ~(uint32_t)DRM_FORMAT_BIG_ENDIAN;
      const uint8_t chars[4] = {
	lowered_format & 0xFF,
	(lowered_format >> 8) & 0xFF,
	(lowered_format >> 16) & 0xFF,
	(lowered_format >> 24) & 0xFF,
      };
      for (int ord = 0; ord < 4; ++ord) {
	if (chars[ord] < (uint8_t)' ' || chars[ord] > (uint8_t)'~') {
	  fprintf(stderr, "Bad DRM format code %s\n", name);
	  return EXIT_FAILURE;
	}
      }
      fprintf(stderr, "Processing format %s (fourcc_code('%c', '%c', '%c', '%c')) (index %lu)\n",
	      name, (char)chars[0], (char)chars[1], (char)chars[2], (char)chars[3], i);
    }
    if (printf("  { .format           = %s\n"
	       "  , .depth            = %" PRIu8 "\n"
	       "  , .num_planes       = %" PRIu8 "\n",
	       name, formats[i].depth, num_planes) < 0) {
      exit(EXIT_FAILURE);
    }
    if (num_planes < 1) {
      fprintf(stderr, "Bad info for format %s: no planes\n", name);
      return EXIT_FAILURE;
    }
    if (num_planes > 4) {
      fprintf(stderr, "Bad info for format %s: too many planes (%" PRIu8 ")\n", name, num_planes);
      return EXIT_FAILURE;
    }
    if (formats[i].vsub < 1) {
      fprintf(stderr, "Bad info for format %s: vsub < 1\n", name);
      return EXIT_FAILURE;
    }
    if (formats[i].hsub < 1) {
      fprintf(stderr, "Bad info for format %s: hsub < 1\n", name);
      return EXIT_FAILURE;
    }
    if (i > 0) {
      assert(format_number != DRM_FORMAT_C1);
    } else {
      assert(format_number == DRM_FORMAT_C1);
    }
#define AMAX(a, b) ((a) > (b) ? (a) : (b))
    assert(formats[0].block_w[0] == 8);
    for (int plane = 0; plane < num_planes; ++plane) {
      uint8_t const char_per_block = formats[i].char_per_block[plane];
      const uint8_t block_width = AMAX(formats[i].block_w[plane], 1);
      const uint8_t block_height = AMAX(formats[i].block_h[plane], 1);
      if (0 && char_per_block * 8 % ((uint32_t)block_width * (uint32_t)block_height) != 0) {
	fprintf(stderr, "Bad info in plane %d for format %s (%" PRIu32 "), row %lu: char per block (%" PRIu8
		") not multiple of width (%" PRIu8 ") * height (%" PRIu8 ")\n",
		plane, name, format_number, i, char_per_block, block_width, block_height);
	return EXIT_FAILURE;
      }
    }
#define X(field) print_buffer(formats[i].field, formats[i].num_planes, #field)
    X(char_per_block);
    X(block_w);
    X(block_h);
#undef X
    if (printf("  , .hsub             = %" PRIu8 "\n"
	       "  , .vsub             = %" PRIu8 "\n"
	       "  , .has_alpha        = %s\n"
	       "  , .is_yuv           = %s\n"
	       "  , .is_color_indexed = %s\n"
	       "  },\n",
	       formats[i].hsub,
	       formats[i].vsub,
	       formats[i].has_alpha ? "true" : "false",
	       formats[i].is_yuv ? "true" : "false",
	       formats[i].is_color_indexed ? "true" : "false") < 0)
      exit(EXIT_FAILURE);
  }
  fputs("};\n", stdout);
  if (fflush(NULL) || ferror(stdout))
    return EXIT_FAILURE;
  return EXIT_SUCCESS;
}
