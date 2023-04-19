module Lanczos
use :: Matrix
use :: VectorOps
implicit none
contains

subroutine lanczoscalc(A, V, T, n)
integer, intent(in) :: n
real*8, dimension(n,n), intent(in) :: A
real*8, dimension(n,n), intent(out) :: V, T
real*8, dimension(n) :: w, wprim
real*8 :: alpha, beta
integer :: i,j

forall (i=1:n, j=1:n)
    T(i,j) = 0
end forall

call orth(V, 1, n)

call mulmatvec(A, V(:,1), wprim, n)
call dot(wprim, V(:,1), alpha, n)
w = wprim - alpha * V(:,1)
T(1,1) = alpha;

do i = 2,n
    call dot(w, w, beta, n)
    beta = sqrt(beta)

    if (beta < 1d-8) then
        call orth(V, i, n)
    else
        V(:,i) = w / beta
    end if

    call mulmatvec(A, V(:,i), wprim, n)

    call dot(wprim, V(:,i), alpha, n)
    
    w = wprim - alpha * V(:,i) - beta * V(:,i-1)

    T(i,i) = alpha
    T(i-1,i) = beta
    T(i,i-1) = beta
end do

end subroutine lanczoscalc

end module Lanczos