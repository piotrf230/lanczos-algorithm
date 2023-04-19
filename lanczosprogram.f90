subroutine minmax(array, min, max, len)
    integer, intent(in) :: len
    real*8, dimension(len), intent(in) :: array
    real*8, intent(out) :: min, max
    integer :: i

    min = array(1)
    max = array(1)
    do i = 2,len
        if (min > array(i)) min = array(i)
        if (max < array(i)) max = array(i)
    end do

end subroutine minmax


program lanczosprogram
use :: VectorOps
use :: Matrix
use :: Lanczos
implicit none

real*8, dimension(:,:), allocatable :: mat, V, T, mat2
real*8, dimension(:), allocatable :: work, w, wa, tdiag
real*8 :: lanmax, lanmin, lapmax, lapmin
integer :: n, i, lwork, info

call loadmatrixfromfile(1, 'Matrix.dat', mat, n)
lwork = 3*n-1
allocate(work(lwork))
allocate(w(n))
allocate(wa(n))
allocate(mat2(n,n))
mat2 = mat

call dsyev('N', 'U', n, mat2, n, wa, work, lwork, info)
deallocate(mat2)

allocate(V(n,n))
allocate(T(n,n))
allocate(tdiag(n))

call lanczoscalc(mat, V, T, n)

call dsyev('N', 'U', n, T, n, w, work, lwork, info)

open(1, file='LanczosOut.dat', status='new', action='write')
write(1,*) 'Lanczos'
do i = 1,n
    write(1,*) w(i)
end do
close(1)

call minmax(wa, lapmin, lapmax, n)
call minmax(w, lanmin, lanmax, n)

write(*,*) 'DSYEV:      ', lapmin, lapmax
write(*,*) 'Lanczos:    ', lanmin, lanmax

end program lanczosprogram