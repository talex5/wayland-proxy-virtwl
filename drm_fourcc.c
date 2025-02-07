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

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <drm/drm_fourcc.h>
#include "drm_fourcc_table.h"
#define ARRAY_SIZE(x) (sizeof(x)/sizeof((x)[0]))

/**
 * drm_format_info_block_width - width in pixels of block.
 * @info: pixel format info
 * @plane: plane index
 *
 * Returns:
 * The width in pixels of a block, depending on the plane index.
 */
unsigned int drm_format_info_block_width(const struct drm_format_info *info,
					 int plane)
{
	if (!info || plane < 0 || plane >= info->num_planes)
		return 0;

	if (!info->block_w[plane])
		return 1;
	return info->block_w[plane];
}

/**
 * drm_format_info_block_height - height in pixels of a block
 * @param info pixel format info
 * @param plane plane index
 *
 * @return The height in pixels of a block, depending on the plane index.
 */
unsigned int drm_format_info_block_height(const struct drm_format_info *info,
					  int plane)
{
	if (!info || plane < 0 || plane >= info->num_planes)
		return 0;

	return info->block_h[plane];
}

/**
 * drm_format_info_bpp - number of bits per pixel
 * @param info pixel format info
 * @param plane plane index
 *
 * @return The actual number of bits per pixel, depending on the plane index.
 */
unsigned int drm_format_info_bpp(const struct drm_format_info *info, int plane)
{
	if (!info || plane < 0 || plane >= info->num_planes)
		return 0;

	return info->char_per_block[plane] * 8 /
		((uint32_t)info->block_h[plane] * (uint32_t)info->block_w[plane]);
}

/**
 * drm_format_info_min_pitch - computes the minimum required pitch in bytes
 * @param info pixel format info
 * @param plane plane index
 * @param buffer_width buffer width in pixels
 *
 * @return
 * The minimum required pitch in bytes for a buffer by taking into consideration
 * the pixel format information and the buffer width.
 */
uint64_t drm_format_info_min_pitch(const struct drm_format_info *info,
				   int plane, uint32_t buffer_width)
{
	if (!info || plane < 0 || plane >= info->num_planes)
		return 0;

	uint64_t width = (uint64_t)buffer_width * info->char_per_block[plane];

	uint64_t divisor = (uint32_t)info->block_h[plane] * (uint32_t)info->block_w[plane];
	if (divisor < 1)
		return 0;
	return (width + (divisor - 1))/divisor;
}

/*
 * Local Variables:
 * c-basic-offset: 8
 * indent-tabs-mode: t
 * c-file-style: "linux"
 * End:
 */
