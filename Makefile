.ONESHELL:
all: drm_fourcc_table.h
	dune build
drm_fourcc_table.h: datum
	./datum > $@.tmp && mv $@.tmp $@
datum: datum.c case.c
	$(CC) datum.c -o $@ -Wall -Wextra $(CFLAGS) $(LDFLAGS) $(LDLIBS) -MD -MP -MF datum.deps
-include datum.deps
case.c: datum.c Makefile
	set -o pipefail && {
		grep -oEw 'DRM_FORMAT_[0-9A-Z_]+' datum.c |
		sort -u |
		LC_ALL=C sed -n 't junk
	:junk
	s/^.*$$/  case &: return "&";/w case.c.tmp
	t finish
	q 1
	:finish'
	}
	mv case.c.tmp case.c
