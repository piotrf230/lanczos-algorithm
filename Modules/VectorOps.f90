module VectorOps
implicit none
contains

subroutine dot(v1, v2, outval, len)
integer, intent(in) :: len
real*8, dimension(len), intent(in) :: v1, v2
real*8, intent(out) :: outval
integer :: i

outval = 0
do i = 1, len
    outval = outval + (v1(i) * v2(i))
end do
end subroutine dot

subroutine norm(v, len)
integer, intent(in) :: len
real*8, dimension(len), intent(inout) :: v
real*8 :: l

call dot(v, v, l, len)
l = sqrt(l)
v = v / l
end subroutine norm

subroutine proj(u, v, vout, len)
integer, intent(in) :: len
real*8, dimension(len), intent(in) :: v, u
real*8, dimension(len), intent(out) :: vout
real*8 :: uv, uu

call dot(u, v, uv, len)
call dot(u, u, uu, len)
vout = (uv / uu) * u

end subroutine proj

subroutine orth(vmat, n, len)
integer, intent(in) :: n, len
real*8, dimension(len,len), intent(inout) :: vmat
real*8, dimension(len) :: tmp, tmpv
integer :: i

if (n == 1) then
    call random_number(vmat(:,1))
else
    call random_number(vmat(:,n))
    tmpv = vmat(:,n)
    do i = 1, n-1
        call proj(vmat(:,i), tmpv, tmp, len)
        vmat(:,n) = vmat(:,n) - tmp
    end do
end if

call norm(vmat(:,n), len)

end subroutine orth

end module VectorOps