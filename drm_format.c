
const struct drm_format_info *
wayland_proxy_virtwl_lookup_format(uint32_t untrusted_format)
{
  const struct drm_format_info *const info = drm_format_info(untrusted_format);
  if (info == NULL) {
    return NULL; /* Unknown format, cannot validate. */
  }

  if (info->hsub < 1 || info->vsub < 1 || info->num_planes < 0 || info->num_planes > 4) {
    /* Corrupt format */
    abort();
  }

  if (info->is_color_indexed) {
    return NULL; /* Color index cannot be used. */
  }

  if (info->block_height > 1) {
    /* It is unclear what the correct stride is for these formats.
     * Should it include the number of pixels? */
    return NULL;
  }
}
