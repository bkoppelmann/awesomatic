program test_recursive_pochhammer
!
!      test the recursive F90 version of the Pochhammer code
!
implicit none
!
!      kind variables
! 
integer, parameter :: LONGreal = selected_real_kind(15,90)
!
!      local parameters
!
integer, parameter :: iteration_max_for_inner_loop = 100
integer, parameter :: iteration_max_for_outer_loop=10000
real (kind = LONGreal), parameter :: zero =0.0
!
!      local variables
!
real (kind = LONGreal) :: pochhammer,z,result,check
integer :: inner_loop_counter,outer_loop_counter,total_number_of_iterations
!
!      external declarations
!
external pochhammer
!
total_number_of_iterations = 0
check = zero
do outer_loop_counter = 1,iteration_max_for_outer_loop
    do inner_loop_counter = 1,iteration_max_for_inner_loop
        z = dble(inner_loop_counter)
        result = pochhammer(z,inner_loop_counter)
	check = check + result
        total_number_of_iterations = total_number_of_iterations + 1
    enddo
enddo
!
write (*,'(/''Recursive F90 Pochhammer test complete for '',i9,'' trys''/   &
&         ''Summation value = '',1pd15.5)') total_number_of_iterations,check
!
end program test_recursive_pochhammer
recursive function pochhammer (z,n) result(pochhammer_symbol_value)
!
!        calculate the Pochhammer symbol (z)
!                                           n
!
!        where z is real and m is an integer.  We require that n be
!        non-negative.
!
!        Pochhammer's symbol is defined as:
!
!        (z)  = 1,
!           0
!
!        (z)  = z(z+1)(z+2) ... (z+n-1)
!           n
!
!        Note that these relations define a recurrence relationship
!           (z)  = (z)    * (z+n-1)
!              n      n-1
!
!        See "Handbook of Mathematical Functions", M. Abramowitz and
!        I. A. Stegun, Dover Publications Inc., 1972, page 256
!
implicit none   
!
!      kind variables
! 
integer, parameter :: LONGreal = selected_real_kind(15,90)
!
!      local parameters
!
real (kind=LONGreal), parameter :: one = 1.0
!
!      local variables
!
real (kind = LONGreal) :: pochhammer_symbol_value,z
integer :: n
!
if (n == 0) then
    pochhammer_symbol_value = one
else
    pochhammer_symbol_value = pochhammer(z,n-1) * (z + real(n-1,LONGreal))
endif
!
end function pochhammer
