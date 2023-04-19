module Matrix
use :: VectorOps
implicit none
contains

subroutine loadmatrixfromfile(index, filename, mat, len)
integer :: r, c, status
real*8, allocatable, intent(out) :: mat(:,:)
real*8 :: a
integer, intent(in) :: index
integer, intent(out) :: len
character(len=10), intent(in) :: filename

open(index, file=filename, status='old', action='read')
read(index,*) len
allocate(mat(len, len))

status = 0
do while (status == 0)
    read(index,*,iostat=status) r, c, a
    mat(r,c) = a
end do

close(index)

end subroutine loadmatrixfromfile



subroutine savematrixtofile(index, filename, mat, len)
integer, intent(in) :: index, len
real*8, dimension(len, len), intent(in) :: mat
character(len=20), intent(in) :: filename
integer :: r, c

open(index, file=filename, status='new', action='write')
write(index,*) len

do r = 1,len
    do c = 1,len
        if (mat(r,c) < 1d-8) write(index,*) r, c, mat(r,c)
    end do
end do

close(index)

end subroutine savematrixtofile



subroutine mulmatvec(m, v, vout, len)
integer, intent(in) :: len
real*8, dimension(len, len), intent(in) :: m
real*8, dimension(len), intent(in) :: v
real*8, dimension(len), intent(out) :: vout
integer :: i

do i = 1,len
    call dot(m(i,:), v, vout(i), len)
end do

end subroutine mulmatvec


subroutine mulvecmat(v, m, vout, len)
integer, intent(in) :: len
real*8, dimension(len, len), intent(in) :: m
real*8, dimension(len), intent(in) :: v
real*8, dimension(len), intent(out) :: vout
integer :: j

do j = 1,len
    call dot(m(:,j), v, vout(j), len)
end do

end subroutine mulvecmat

end module Matrix