module modulo_precision

use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use, intrinsic :: iso_fortran_env, only : real32, real64, real128
use, intrinsic :: iso_fortran_env, only : iostat_end

implicit none

integer(kind=int8), parameter :: is = int8, id = int16, il = int32, ix = int64
integer(kind=int8), parameter :: rs = real32, rd = real64, rl = real128

integer, parameter :: wp=rd

end module modulo_precision
