This is the second submission for 0.1.2. The previous one failed
because of two NOTEs:
1. moved URLs (now fixed).
2. "'cc' is not on the path" on Windows only. I was able to
   reproduce this on `devtools::check_win_devel()` but I didn't find any
   find any related discussion in the R-package-devel mailing list.

