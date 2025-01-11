#include <stdbool.h>
#include <drm/drm_fourcc.h>
#include "drm_fourcc.h"

struct drm_format_info formats[118] = {
  { .format           = DRM_FORMAT_C1
  , .depth            = 1
  , .num_planes       = 1
  , .char_per_block   = { 1, 0, 0, 0 }
  , .block_w          = { 8, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = true
  },
  { .format           = DRM_FORMAT_C2
  , .depth            = 2
  , .num_planes       = 1
  , .char_per_block   = { 1, 0, 0, 0 }
  , .block_w          = { 4, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = true
  },
  { .format           = DRM_FORMAT_C4
  , .depth            = 4
  , .num_planes       = 1
  , .char_per_block   = { 1, 0, 0, 0 }
  , .block_w          = { 2, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = true
  },
  { .format           = DRM_FORMAT_C8
  , .depth            = 8
  , .num_planes       = 1
  , .char_per_block   = { 1, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = true
  },
  { .format           = DRM_FORMAT_D1
  , .depth            = 1
  , .num_planes       = 1
  , .char_per_block   = { 1, 0, 0, 0 }
  , .block_w          = { 8, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_D2
  , .depth            = 2
  , .num_planes       = 1
  , .char_per_block   = { 1, 0, 0, 0 }
  , .block_w          = { 4, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_D4
  , .depth            = 4
  , .num_planes       = 1
  , .char_per_block   = { 1, 0, 0, 0 }
  , .block_w          = { 2, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_D8
  , .depth            = 8
  , .num_planes       = 1
  , .char_per_block   = { 1, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_R1
  , .depth            = 1
  , .num_planes       = 1
  , .char_per_block   = { 1, 0, 0, 0 }
  , .block_w          = { 8, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_R2
  , .depth            = 2
  , .num_planes       = 1
  , .char_per_block   = { 1, 0, 0, 0 }
  , .block_w          = { 4, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_R4
  , .depth            = 4
  , .num_planes       = 1
  , .char_per_block   = { 1, 0, 0, 0 }
  , .block_w          = { 2, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_R8
  , .depth            = 8
  , .num_planes       = 1
  , .char_per_block   = { 1, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_R10
  , .depth            = 10
  , .num_planes       = 1
  , .char_per_block   = { 2, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_R12
  , .depth            = 12
  , .num_planes       = 1
  , .char_per_block   = { 2, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_RGB332
  , .depth            = 8
  , .num_planes       = 1
  , .char_per_block   = { 1, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_BGR233
  , .depth            = 8
  , .num_planes       = 1
  , .char_per_block   = { 1, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_XRGB4444
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 2, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_XBGR4444
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 2, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_RGBX4444
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 2, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_BGRX4444
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 2, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_ARGB4444
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 2, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_ABGR4444
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 2, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_RGBA4444
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 2, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_BGRA4444
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 2, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_XRGB1555
  , .depth            = 15
  , .num_planes       = 1
  , .char_per_block   = { 2, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_XBGR1555
  , .depth            = 15
  , .num_planes       = 1
  , .char_per_block   = { 2, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_RGBX5551
  , .depth            = 15
  , .num_planes       = 1
  , .char_per_block   = { 2, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_BGRX5551
  , .depth            = 15
  , .num_planes       = 1
  , .char_per_block   = { 2, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_ARGB1555
  , .depth            = 15
  , .num_planes       = 1
  , .char_per_block   = { 2, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_ABGR1555
  , .depth            = 15
  , .num_planes       = 1
  , .char_per_block   = { 2, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_RGBA5551
  , .depth            = 15
  , .num_planes       = 1
  , .char_per_block   = { 2, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_BGRA5551
  , .depth            = 15
  , .num_planes       = 1
  , .char_per_block   = { 2, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_RGB565
  , .depth            = 16
  , .num_planes       = 1
  , .char_per_block   = { 2, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_BGR565
  , .depth            = 16
  , .num_planes       = 1
  , .char_per_block   = { 2, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_RGB888
  , .depth            = 24
  , .num_planes       = 1
  , .char_per_block   = { 3, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_BGR888
  , .depth            = 24
  , .num_planes       = 1
  , .char_per_block   = { 3, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_XRGB8888
  , .depth            = 24
  , .num_planes       = 1
  , .char_per_block   = { 4, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_XBGR8888
  , .depth            = 24
  , .num_planes       = 1
  , .char_per_block   = { 4, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_RGBX8888
  , .depth            = 24
  , .num_planes       = 1
  , .char_per_block   = { 4, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_BGRX8888
  , .depth            = 24
  , .num_planes       = 1
  , .char_per_block   = { 4, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_RGB565_A8
  , .depth            = 24
  , .num_planes       = 2
  , .char_per_block   = { 2, 1, 0, 0 }
  , .block_w          = { 1, 1, 0, 0 }
  , .block_h          = { 1, 1, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_BGR565_A8
  , .depth            = 24
  , .num_planes       = 2
  , .char_per_block   = { 2, 1, 0, 0 }
  , .block_w          = { 1, 1, 0, 0 }
  , .block_h          = { 1, 1, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_XRGB2101010
  , .depth            = 30
  , .num_planes       = 1
  , .char_per_block   = { 4, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_XBGR2101010
  , .depth            = 30
  , .num_planes       = 1
  , .char_per_block   = { 4, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_RGBX1010102
  , .depth            = 30
  , .num_planes       = 1
  , .char_per_block   = { 4, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_BGRX1010102
  , .depth            = 30
  , .num_planes       = 1
  , .char_per_block   = { 4, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_ARGB2101010
  , .depth            = 30
  , .num_planes       = 1
  , .char_per_block   = { 4, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_ABGR2101010
  , .depth            = 30
  , .num_planes       = 1
  , .char_per_block   = { 4, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_RGBA1010102
  , .depth            = 30
  , .num_planes       = 1
  , .char_per_block   = { 4, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_BGRA1010102
  , .depth            = 30
  , .num_planes       = 1
  , .char_per_block   = { 4, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_ARGB8888
  , .depth            = 32
  , .num_planes       = 1
  , .char_per_block   = { 4, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_ABGR8888
  , .depth            = 32
  , .num_planes       = 1
  , .char_per_block   = { 4, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_RGBA8888
  , .depth            = 32
  , .num_planes       = 1
  , .char_per_block   = { 4, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_BGRA8888
  , .depth            = 32
  , .num_planes       = 1
  , .char_per_block   = { 4, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_XRGB16161616F
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 8, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_XBGR16161616F
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 8, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_ARGB16161616F
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 8, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_ABGR16161616F
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 8, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_AXBXGXRX106106106106
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 8, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_XRGB16161616
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 8, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_XBGR16161616
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 8, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_ARGB16161616
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 8, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_ABGR16161616
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 8, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_RGB888_A8
  , .depth            = 32
  , .num_planes       = 2
  , .char_per_block   = { 3, 1, 0, 0 }
  , .block_w          = { 1, 1, 0, 0 }
  , .block_h          = { 1, 1, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_BGR888_A8
  , .depth            = 32
  , .num_planes       = 2
  , .char_per_block   = { 3, 1, 0, 0 }
  , .block_w          = { 1, 1, 0, 0 }
  , .block_h          = { 1, 1, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_XRGB8888_A8
  , .depth            = 32
  , .num_planes       = 2
  , .char_per_block   = { 4, 1, 0, 0 }
  , .block_w          = { 1, 1, 0, 0 }
  , .block_h          = { 1, 1, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_XBGR8888_A8
  , .depth            = 32
  , .num_planes       = 2
  , .char_per_block   = { 4, 1, 0, 0 }
  , .block_w          = { 1, 1, 0, 0 }
  , .block_h          = { 1, 1, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_RGBX8888_A8
  , .depth            = 32
  , .num_planes       = 2
  , .char_per_block   = { 4, 1, 0, 0 }
  , .block_w          = { 1, 1, 0, 0 }
  , .block_h          = { 1, 1, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_BGRX8888_A8
  , .depth            = 32
  , .num_planes       = 2
  , .char_per_block   = { 4, 1, 0, 0 }
  , .block_w          = { 1, 1, 0, 0 }
  , .block_h          = { 1, 1, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = false
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_YUV410
  , .depth            = 0
  , .num_planes       = 3
  , .char_per_block   = { 1, 1, 1, 0 }
  , .block_w          = { 1, 1, 1, 0 }
  , .block_h          = { 1, 1, 1, 0 }
  , .hsub             = 4
  , .vsub             = 4
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_YVU410
  , .depth            = 0
  , .num_planes       = 3
  , .char_per_block   = { 1, 1, 1, 0 }
  , .block_w          = { 1, 1, 1, 0 }
  , .block_h          = { 1, 1, 1, 0 }
  , .hsub             = 4
  , .vsub             = 4
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_YUV411
  , .depth            = 0
  , .num_planes       = 3
  , .char_per_block   = { 1, 1, 1, 0 }
  , .block_w          = { 1, 1, 1, 0 }
  , .block_h          = { 1, 1, 1, 0 }
  , .hsub             = 4
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_YVU411
  , .depth            = 0
  , .num_planes       = 3
  , .char_per_block   = { 1, 1, 1, 0 }
  , .block_w          = { 1, 1, 1, 0 }
  , .block_h          = { 1, 1, 1, 0 }
  , .hsub             = 4
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_YUV420
  , .depth            = 0
  , .num_planes       = 3
  , .char_per_block   = { 1, 1, 1, 0 }
  , .block_w          = { 1, 1, 1, 0 }
  , .block_h          = { 1, 1, 1, 0 }
  , .hsub             = 2
  , .vsub             = 2
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_YVU420
  , .depth            = 0
  , .num_planes       = 3
  , .char_per_block   = { 1, 1, 1, 0 }
  , .block_w          = { 1, 1, 1, 0 }
  , .block_h          = { 1, 1, 1, 0 }
  , .hsub             = 2
  , .vsub             = 2
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_YUV422
  , .depth            = 0
  , .num_planes       = 3
  , .char_per_block   = { 1, 1, 1, 0 }
  , .block_w          = { 1, 1, 1, 0 }
  , .block_h          = { 1, 1, 1, 0 }
  , .hsub             = 2
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_YVU422
  , .depth            = 0
  , .num_planes       = 3
  , .char_per_block   = { 1, 1, 1, 0 }
  , .block_w          = { 1, 1, 1, 0 }
  , .block_h          = { 1, 1, 1, 0 }
  , .hsub             = 2
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_YUV444
  , .depth            = 0
  , .num_planes       = 3
  , .char_per_block   = { 1, 1, 1, 0 }
  , .block_w          = { 1, 1, 1, 0 }
  , .block_h          = { 1, 1, 1, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_YVU444
  , .depth            = 0
  , .num_planes       = 3
  , .char_per_block   = { 1, 1, 1, 0 }
  , .block_w          = { 1, 1, 1, 0 }
  , .block_h          = { 1, 1, 1, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_NV12
  , .depth            = 0
  , .num_planes       = 2
  , .char_per_block   = { 1, 2, 0, 0 }
  , .block_w          = { 1, 1, 0, 0 }
  , .block_h          = { 1, 1, 0, 0 }
  , .hsub             = 2
  , .vsub             = 2
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_NV21
  , .depth            = 0
  , .num_planes       = 2
  , .char_per_block   = { 1, 2, 0, 0 }
  , .block_w          = { 1, 1, 0, 0 }
  , .block_h          = { 1, 1, 0, 0 }
  , .hsub             = 2
  , .vsub             = 2
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_NV16
  , .depth            = 0
  , .num_planes       = 2
  , .char_per_block   = { 1, 2, 0, 0 }
  , .block_w          = { 1, 1, 0, 0 }
  , .block_h          = { 1, 1, 0, 0 }
  , .hsub             = 2
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_NV61
  , .depth            = 0
  , .num_planes       = 2
  , .char_per_block   = { 1, 2, 0, 0 }
  , .block_w          = { 1, 1, 0, 0 }
  , .block_h          = { 1, 1, 0, 0 }
  , .hsub             = 2
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_NV24
  , .depth            = 0
  , .num_planes       = 2
  , .char_per_block   = { 1, 2, 0, 0 }
  , .block_w          = { 1, 1, 0, 0 }
  , .block_h          = { 1, 1, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_NV42
  , .depth            = 0
  , .num_planes       = 2
  , .char_per_block   = { 1, 2, 0, 0 }
  , .block_w          = { 1, 1, 0, 0 }
  , .block_h          = { 1, 1, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_YUYV
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 2, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 2
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_YVYU
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 2, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 2
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_UYVY
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 2, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 2
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_VYUY
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 2, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 2
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_XYUV8888
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 4, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_VUY888
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 3, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_AYUV
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 4, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_Y210
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 4, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 2
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_Y212
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 4, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 2
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_Y216
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 4, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 2
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_Y410
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 4, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_Y412
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 8, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_Y416
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 8, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = true
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_XVYU2101010
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 4, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_XVYU12_16161616
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 8, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_XVYU16161616
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 8, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_Y0L0
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 8, 0, 0, 0 }
  , .block_w          = { 2, 0, 0, 0 }
  , .block_h          = { 2, 0, 0, 0 }
  , .hsub             = 2
  , .vsub             = 2
  , .has_alpha        = true
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_X0L0
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 8, 0, 0, 0 }
  , .block_w          = { 2, 0, 0, 0 }
  , .block_h          = { 2, 0, 0, 0 }
  , .hsub             = 2
  , .vsub             = 2
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_Y0L2
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 8, 0, 0, 0 }
  , .block_w          = { 2, 0, 0, 0 }
  , .block_h          = { 2, 0, 0, 0 }
  , .hsub             = 2
  , .vsub             = 2
  , .has_alpha        = true
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_X0L2
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 8, 0, 0, 0 }
  , .block_w          = { 2, 0, 0, 0 }
  , .block_h          = { 2, 0, 0, 0 }
  , .hsub             = 2
  , .vsub             = 2
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_P010
  , .depth            = 0
  , .num_planes       = 2
  , .char_per_block   = { 2, 4, 0, 0 }
  , .block_w          = { 1, 1, 0, 0 }
  , .block_h          = { 1, 1, 0, 0 }
  , .hsub             = 2
  , .vsub             = 2
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_P012
  , .depth            = 0
  , .num_planes       = 2
  , .char_per_block   = { 2, 4, 0, 0 }
  , .block_w          = { 1, 1, 0, 0 }
  , .block_h          = { 1, 1, 0, 0 }
  , .hsub             = 2
  , .vsub             = 2
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_P016
  , .depth            = 0
  , .num_planes       = 2
  , .char_per_block   = { 2, 4, 0, 0 }
  , .block_w          = { 1, 1, 0, 0 }
  , .block_h          = { 1, 1, 0, 0 }
  , .hsub             = 2
  , .vsub             = 2
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_P210
  , .depth            = 0
  , .num_planes       = 2
  , .char_per_block   = { 2, 4, 0, 0 }
  , .block_w          = { 1, 1, 0, 0 }
  , .block_h          = { 1, 1, 0, 0 }
  , .hsub             = 2
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_VUY101010
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 1, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_YUV420_8BIT
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 1, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 2
  , .vsub             = 2
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_YUV420_10BIT
  , .depth            = 0
  , .num_planes       = 1
  , .char_per_block   = { 1, 0, 0, 0 }
  , .block_w          = { 1, 0, 0, 0 }
  , .block_h          = { 1, 0, 0, 0 }
  , .hsub             = 2
  , .vsub             = 2
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_NV15
  , .depth            = 0
  , .num_planes       = 2
  , .char_per_block   = { 5, 5, 0, 0 }
  , .block_w          = { 4, 2, 0, 0 }
  , .block_h          = { 1, 1, 0, 0 }
  , .hsub             = 2
  , .vsub             = 2
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_NV20
  , .depth            = 0
  , .num_planes       = 2
  , .char_per_block   = { 5, 5, 0, 0 }
  , .block_w          = { 4, 2, 0, 0 }
  , .block_h          = { 1, 1, 0, 0 }
  , .hsub             = 2
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_NV30
  , .depth            = 0
  , .num_planes       = 2
  , .char_per_block   = { 5, 5, 0, 0 }
  , .block_w          = { 4, 2, 0, 0 }
  , .block_h          = { 1, 1, 0, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_Q410
  , .depth            = 0
  , .num_planes       = 3
  , .char_per_block   = { 2, 2, 2, 0 }
  , .block_w          = { 1, 1, 1, 0 }
  , .block_h          = { 1, 1, 1, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_Q401
  , .depth            = 0
  , .num_planes       = 3
  , .char_per_block   = { 2, 2, 2, 0 }
  , .block_w          = { 1, 1, 1, 0 }
  , .block_h          = { 1, 1, 1, 0 }
  , .hsub             = 1
  , .vsub             = 1
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
  { .format           = DRM_FORMAT_P030
  , .depth            = 0
  , .num_planes       = 2
  , .char_per_block   = { 4, 8, 0, 0 }
  , .block_w          = { 3, 3, 0, 0 }
  , .block_h          = { 1, 1, 0, 0 }
  , .hsub             = 2
  , .vsub             = 2
  , .has_alpha        = false
  , .is_yuv           = true
  , .is_color_indexed = false
  },
};
/**
 * drm_format_info - query information for a given format
 * @param format pixel format (DRM_FORMAT_*)
 *
 * @return
 * The instance of struct drm_format_info that describes the pixel format, or
 * NULL if the format is unsupported.
 */
const struct drm_format_info *drm_format_info(uint32_t format)
{
  switch (format) {
  case DRM_FORMAT_C1: return formats + 0;
  case DRM_FORMAT_C2: return formats + 1;
  case DRM_FORMAT_C4: return formats + 2;
  case DRM_FORMAT_C8: return formats + 3;
  case DRM_FORMAT_D1: return formats + 4;
  case DRM_FORMAT_D2: return formats + 5;
  case DRM_FORMAT_D4: return formats + 6;
  case DRM_FORMAT_D8: return formats + 7;
  case DRM_FORMAT_R1: return formats + 8;
  case DRM_FORMAT_R2: return formats + 9;
  case DRM_FORMAT_R4: return formats + 10;
  case DRM_FORMAT_R8: return formats + 11;
  case DRM_FORMAT_R10: return formats + 12;
  case DRM_FORMAT_R12: return formats + 13;
  case DRM_FORMAT_RGB332: return formats + 14;
  case DRM_FORMAT_BGR233: return formats + 15;
  case DRM_FORMAT_XRGB4444: return formats + 16;
  case DRM_FORMAT_XBGR4444: return formats + 17;
  case DRM_FORMAT_RGBX4444: return formats + 18;
  case DRM_FORMAT_BGRX4444: return formats + 19;
  case DRM_FORMAT_ARGB4444: return formats + 20;
  case DRM_FORMAT_ABGR4444: return formats + 21;
  case DRM_FORMAT_RGBA4444: return formats + 22;
  case DRM_FORMAT_BGRA4444: return formats + 23;
  case DRM_FORMAT_XRGB1555: return formats + 24;
  case DRM_FORMAT_XBGR1555: return formats + 25;
  case DRM_FORMAT_RGBX5551: return formats + 26;
  case DRM_FORMAT_BGRX5551: return formats + 27;
  case DRM_FORMAT_ARGB1555: return formats + 28;
  case DRM_FORMAT_ABGR1555: return formats + 29;
  case DRM_FORMAT_RGBA5551: return formats + 30;
  case DRM_FORMAT_BGRA5551: return formats + 31;
  case DRM_FORMAT_RGB565: return formats + 32;
  case DRM_FORMAT_BGR565: return formats + 33;
  case DRM_FORMAT_RGB888: return formats + 34;
  case DRM_FORMAT_BGR888: return formats + 35;
  case DRM_FORMAT_XRGB8888: return formats + 36;
  case DRM_FORMAT_XBGR8888: return formats + 37;
  case DRM_FORMAT_RGBX8888: return formats + 38;
  case DRM_FORMAT_BGRX8888: return formats + 39;
  case DRM_FORMAT_RGB565_A8: return formats + 40;
  case DRM_FORMAT_BGR565_A8: return formats + 41;
  case DRM_FORMAT_XRGB2101010: return formats + 42;
  case DRM_FORMAT_XBGR2101010: return formats + 43;
  case DRM_FORMAT_RGBX1010102: return formats + 44;
  case DRM_FORMAT_BGRX1010102: return formats + 45;
  case DRM_FORMAT_ARGB2101010: return formats + 46;
  case DRM_FORMAT_ABGR2101010: return formats + 47;
  case DRM_FORMAT_RGBA1010102: return formats + 48;
  case DRM_FORMAT_BGRA1010102: return formats + 49;
  case DRM_FORMAT_ARGB8888: return formats + 50;
  case DRM_FORMAT_ABGR8888: return formats + 51;
  case DRM_FORMAT_RGBA8888: return formats + 52;
  case DRM_FORMAT_BGRA8888: return formats + 53;
  case DRM_FORMAT_XRGB16161616F: return formats + 54;
  case DRM_FORMAT_XBGR16161616F: return formats + 55;
  case DRM_FORMAT_ARGB16161616F: return formats + 56;
  case DRM_FORMAT_ABGR16161616F: return formats + 57;
  case DRM_FORMAT_AXBXGXRX106106106106: return formats + 58;
  case DRM_FORMAT_XRGB16161616: return formats + 59;
  case DRM_FORMAT_XBGR16161616: return formats + 60;
  case DRM_FORMAT_ARGB16161616: return formats + 61;
  case DRM_FORMAT_ABGR16161616: return formats + 62;
  case DRM_FORMAT_RGB888_A8: return formats + 63;
  case DRM_FORMAT_BGR888_A8: return formats + 64;
  case DRM_FORMAT_XRGB8888_A8: return formats + 65;
  case DRM_FORMAT_XBGR8888_A8: return formats + 66;
  case DRM_FORMAT_RGBX8888_A8: return formats + 67;
  case DRM_FORMAT_BGRX8888_A8: return formats + 68;
  case DRM_FORMAT_YUV410: return formats + 69;
  case DRM_FORMAT_YVU410: return formats + 70;
  case DRM_FORMAT_YUV411: return formats + 71;
  case DRM_FORMAT_YVU411: return formats + 72;
  case DRM_FORMAT_YUV420: return formats + 73;
  case DRM_FORMAT_YVU420: return formats + 74;
  case DRM_FORMAT_YUV422: return formats + 75;
  case DRM_FORMAT_YVU422: return formats + 76;
  case DRM_FORMAT_YUV444: return formats + 77;
  case DRM_FORMAT_YVU444: return formats + 78;
  case DRM_FORMAT_NV12: return formats + 79;
  case DRM_FORMAT_NV21: return formats + 80;
  case DRM_FORMAT_NV16: return formats + 81;
  case DRM_FORMAT_NV61: return formats + 82;
  case DRM_FORMAT_NV24: return formats + 83;
  case DRM_FORMAT_NV42: return formats + 84;
  case DRM_FORMAT_YUYV: return formats + 85;
  case DRM_FORMAT_YVYU: return formats + 86;
  case DRM_FORMAT_UYVY: return formats + 87;
  case DRM_FORMAT_VYUY: return formats + 88;
  case DRM_FORMAT_XYUV8888: return formats + 89;
  case DRM_FORMAT_VUY888: return formats + 90;
  case DRM_FORMAT_AYUV: return formats + 91;
  case DRM_FORMAT_Y210: return formats + 92;
  case DRM_FORMAT_Y212: return formats + 93;
  case DRM_FORMAT_Y216: return formats + 94;
  case DRM_FORMAT_Y410: return formats + 95;
  case DRM_FORMAT_Y412: return formats + 96;
  case DRM_FORMAT_Y416: return formats + 97;
  case DRM_FORMAT_XVYU2101010: return formats + 98;
  case DRM_FORMAT_XVYU12_16161616: return formats + 99;
  case DRM_FORMAT_XVYU16161616: return formats + 100;
  case DRM_FORMAT_Y0L0: return formats + 101;
  case DRM_FORMAT_X0L0: return formats + 102;
  case DRM_FORMAT_Y0L2: return formats + 103;
  case DRM_FORMAT_X0L2: return formats + 104;
  case DRM_FORMAT_P010: return formats + 105;
  case DRM_FORMAT_P012: return formats + 106;
  case DRM_FORMAT_P016: return formats + 107;
  case DRM_FORMAT_P210: return formats + 108;
  case DRM_FORMAT_VUY101010: return formats + 109;
  case DRM_FORMAT_YUV420_8BIT: return formats + 110;
  case DRM_FORMAT_YUV420_10BIT: return formats + 111;
  case DRM_FORMAT_NV15: return formats + 112;
  case DRM_FORMAT_NV20: return formats + 113;
  case DRM_FORMAT_NV30: return formats + 114;
  case DRM_FORMAT_Q410: return formats + 115;
  case DRM_FORMAT_Q401: return formats + 116;
  case DRM_FORMAT_P030: return formats + 117;
  default: return NULL;
  }
}
