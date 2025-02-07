#ifndef WAYLAND_PROXY_VIRTWL_DRM_FORMAT_H
#define WAYLAND_PROXY_VIRTWL_DRM_FORMAT_H

/**
 * Look up a DRM format.
 *
 * @param untrusted_format The fourcc code of the format.  It is safe
 *                         to pass unvalidated and untrusted input for this parameter.
 * @return Pointer to format information, or NULL if the format
 * is unacceptable.
 */
const struct drm_format_info *
wayland_proxy_virtwl_lookup_format(uint32_t untrusted_format);


#endif // !defined WAYLAND_PROXY_VIRTWL_DRM_FORMAT_H
