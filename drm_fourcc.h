/*
 * Copyright (c) 2016 Laurent Pinchart <laurent.pinchart@ideasonboard.com>
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
#ifndef __DRM_FOURCC_H__
#define __DRM_FOURCC_H__

#include <stdint.h>
#include <drm/drm_fourcc.h>

/**
 * DRM_FORMAT_MAX_PLANES - maximum number of planes a DRM format can have
 */
#define DRM_FORMAT_MAX_PLANES	4u

/*
 * DRM formats are little endian.  Define host endian variants for the
 * most common formats here, to reduce the #ifdefs needed in drivers.
 *
 * Note that the DRM_FORMAT_BIG_ENDIAN flag should only be used in
 * case the format can't be specified otherwise, so we don't end up
 * with two values describing the same format.
 */
#ifdef __BIG_ENDIAN
# define DRM_FORMAT_HOST_XRGB1555     (DRM_FORMAT_XRGB1555         |	\
				       DRM_FORMAT_BIG_ENDIAN)
# define DRM_FORMAT_HOST_RGB565       (DRM_FORMAT_RGB565           |	\
				       DRM_FORMAT_BIG_ENDIAN)
# define DRM_FORMAT_HOST_XRGB8888     DRM_FORMAT_BGRX8888
# define DRM_FORMAT_HOST_ARGB8888     DRM_FORMAT_BGRA8888
#else
# define DRM_FORMAT_HOST_XRGB1555     DRM_FORMAT_XRGB1555
# define DRM_FORMAT_HOST_RGB565       DRM_FORMAT_RGB565
# define DRM_FORMAT_HOST_XRGB8888     DRM_FORMAT_XRGB8888
# define DRM_FORMAT_HOST_ARGB8888     DRM_FORMAT_ARGB8888
#endif

struct drm_device;
struct drm_mode_fb_cmd2;

/**
 * struct drm_format_info - information about a DRM format
 */
struct drm_format_info {
	/** @format: 4CC format identifier (DRM_FORMAT_*) */
	uint32_t format;

	/**
	 * @depth:
	 *
	 * Color depth (number of bits per pixel excluding padding bits),
	 * valid for a subset of RGB formats only. This is a legacy field, do
	 * not use in new code and set to 0 for new formats.
	 */
	uint8_t depth;

	/** @num_planes: Number of color planes (1 to 3) */
	uint8_t num_planes;

	union {
		/**
		 * @cpp:
		 *
		 * Number of bytes per pixel (per plane), this is aliased with
		 * @char_per_block. It is deprecated in favour of using the
		 * triplet @char_per_block, @block_w, @block_h for better
		 * describing the pixel format.
		 */
		uint8_t cpp[DRM_FORMAT_MAX_PLANES];

		/**
		 * @char_per_block:
		 *
		 * Number of bytes per block (per plane), where blocks are
		 * defined as a rectangle of pixels which are stored next to
		 * each other in a byte aligned memory region. Together with
		 * @block_w and @block_h this is used to properly describe tiles
		 * in tiled formats or to describe groups of pixels in packed
		 * formats for which the memory needed for a single pixel is not
		 * byte aligned.
		 *
		 * @cpp has been kept for historical reasons because there are
		 * a lot of places in drivers where it's used. In drm core for
		 * generic code paths the preferred way is to use
		 * @char_per_block, drm_format_info_block_width() and
		 * drm_format_info_block_height() which allows handling both
		 * block and non-block formats in the same way.
		 *
		 * For formats that are intended to be used only with non-linear
		 * modifiers both @cpp and @char_per_block must be 0 in the
		 * generic format table. Drivers could supply accurate
		 * information from their drm_mode_config.get_format_info hook
		 * if they want the core to be validating the pitch.
		 */
		uint8_t char_per_block[DRM_FORMAT_MAX_PLANES];
	};

	/**
	 * @block_w:
	 *
	 * Block width in pixels, this is intended to be accessed through
	 * drm_format_info_block_width()
	 */
	uint8_t block_w[DRM_FORMAT_MAX_PLANES];

	/**
	 * @block_h:
	 *
	 * Block height in pixels, this is intended to be accessed through
	 * drm_format_info_block_height()
	 */
	uint8_t block_h[DRM_FORMAT_MAX_PLANES];

	/** @hsub: Horizontal chroma subsampling factor */
	uint8_t hsub;
	/** @vsub: Vertical chroma subsampling factor */
	uint8_t vsub;

	/** @has_alpha: Does the format embeds an alpha component? */
	bool has_alpha;

	/** @is_yuv: Is it a YUV format? */
	bool is_yuv;

	/** @is_color_indexed: Is it a color-indexed format? */
	bool is_color_indexed;
};

/**
 * drm_format_info_is_yuv_packed - check that the format info matches a YUV
 * format with data laid in a single plane
 * @info: format info
 *
 * Returns:
 * A boolean indicating whether the format info matches a packed YUV format.
 */
static inline bool
drm_format_info_is_yuv_packed(const struct drm_format_info *info)
{
	return info->is_yuv && info->num_planes == 1;
}

/**
 * drm_format_info_is_yuv_semiplanar - check that the format info matches a YUV
 * format with data laid in two planes (luminance and chrominance).
 * @param info format info
 *
 * @return
 * A boolean indicating whether the format info matches a semiplanar YUV format.
 */
static inline bool
drm_format_info_is_yuv_semiplanar(const struct drm_format_info *info)
{
	return info->is_yuv && info->num_planes == 2;
}

/**
 * drm_format_info_is_yuv_planar - check that the format info matches a YUV
 * format with data laid in three planes (one for each YUV component).
 * @param info format info
 *
 * @return
 * A boolean indicating whether the format info matches a planar YUV format.
 */
static inline bool
drm_format_info_is_yuv_planar(const struct drm_format_info *info)
{
	return info->is_yuv && info->num_planes == 3;
}

/**
 * drm_format_info_is_yuv_sampling_410 - check that the format info matches a
 * YUV format with 4:1:0 sub-sampling
 * @param info format info
 *
 * @return
 * A boolean indicating whether the format info matches a YUV format with 4:1:0
 * sub-sampling.
 */
static inline bool
drm_format_info_is_yuv_sampling_410(const struct drm_format_info *info)
{
	return info->is_yuv && info->hsub == 4 && info->vsub == 4;
}

/**
 * drm_format_info_is_yuv_sampling_411 - check that the format info matches a
 * YUV format with 4:1:1 sub-sampling
 * @param info: format info
 *
 * @return
 * A boolean indicating whether the format info matches a YUV format with 4:1:1
 * sub-sampling.
 */
static inline bool
drm_format_info_is_yuv_sampling_411(const struct drm_format_info *info)
{
	return info->is_yuv && info->hsub == 4 && info->vsub == 1;
}

/**
 * drm_format_info_is_yuv_sampling_420 - check that the format info matches a
 * YUV format with 4:2:0 sub-sampling
 * @param info: format info
 *
 * @return
 * A boolean indicating whether the format info matches a YUV format with 4:2:0
 * sub-sampling.
 */
static inline bool
drm_format_info_is_yuv_sampling_420(const struct drm_format_info *info)
{
	return info->is_yuv && info->hsub == 2 && info->vsub == 2;
}

/**
 * drm_format_info_is_yuv_sampling_422 - check that the format info matches a
 * YUV format with 4:2:2 sub-sampling
 * @info: format info
 *
 * Returns:
 * A boolean indicating whether the format info matches a YUV format with 4:2:2
 * sub-sampling.
 */
static inline bool
drm_format_info_is_yuv_sampling_422(const struct drm_format_info *info)
{
	return info->is_yuv && info->hsub == 2 && info->vsub == 1;
}

/**
 * drm_format_info_is_yuv_sampling_444 - check that the format info matches a
 * YUV format with 4:4:4 sub-sampling
 * @param info: format info
 *
 * @return
 * A boolean indicating whether the format info matches a YUV format with 4:4:4
 * sub-sampling.
 */
static inline bool
drm_format_info_is_yuv_sampling_444(const struct drm_format_info *info)
{
	return info->is_yuv && info->hsub == 1 && info->vsub == 1;
}

/**
 * drm_format_info_plane_width - width of the plane given the first plane
 * @param info: pixel format info
 * @param width: width of the first plane
 * @param plane: plane index
 *
 * @return
 * The width of plane, given that the width of the first plane is width.
 */
static inline uint64_t
drm_format_info_plane_width(const struct drm_format_info *info, uint32_t width,
			    int plane)
{
	if (!info || plane < 0 || plane >= info->num_planes)
		return 0;

	if (plane == 0 || info->hsub < 1)
		return width;

	return ((uint64_t)width + ((uint64_t)info->hsub - UINT64_C(1))) / info->hsub;
}

/**
 * drm_format_info_plane_height - height of the plane given the first plane
 * @param info: pixel format info
 * @param height: height of the first plane
 * @param plane: plane index
 *
 * @return
 * The height of plane, given that the height of the first plane is height.
 */
static inline
uint64_t drm_format_info_plane_height(const struct drm_format_info *info, uint64_t height,
				 int plane)
{
	if (!info || plane < 0 || plane >= info->num_planes)
		return 0;

	if (plane == 0 || info->vsub < 1)
		return height;

	return ((uint64_t)height + ((uint64_t)info->vsub - UINT64_C(1))) / info->vsub;
}

const struct drm_format_info *__drm_format_info(uint32_t format);
const struct drm_format_info *drm_format_info(uint32_t format);
unsigned int drm_format_info_block_width(const struct drm_format_info *info,
					 int plane);
unsigned int drm_format_info_block_height(const struct drm_format_info *info,
					  int plane);
unsigned int drm_format_info_bpp(const struct drm_format_info *info, int plane);
uint64_t drm_format_info_min_pitch(const struct drm_format_info *info,
				   int plane, unsigned int buffer_width);

#endif /* __DRM_FOURCC_H__ */

/*
 * Local Variables:
 * c-basic-offset: 8
 * indent-tabs-mode: t
 * c-file-style: linux
 */
