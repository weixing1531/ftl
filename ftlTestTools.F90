module ftlTestToolsModule
	implicit none

	integer, protected, save :: num_asserts = 0
	integer, protected, save :: num_failed = 0
contains
#if defined(NAGFOR)
subroutine assert(cond, file, line)
#else
subroutine assert(cond, str, file, line)
#endif
	logical , intent(in) :: cond
#if !defined(NAGFOR)
	character(len=*), intent(in) :: str
#endif
	character(len=*), intent(in) :: file
	integer , intent(in) :: line

	num_asserts = num_asserts + 1

	if (.not.cond) then
#if defined(NAGFOR)
		write (*,'(A,A,A,I0,A,A)') 'Failed assertion at ',file,'(',line,')'
#else
		write (*,'(A,A,A,I0,A,A)') 'Failed assertion at ',file,'(',line,'): ',str
#endif
		num_failed = num_failed + 1
#if !defined(NAGFOR)
		if (str == '.false.') then
			write (*,'(A)') '(This was a dummy assertion to test if assertions themselves work ...)'
			num_asserts = num_asserts - 1
			num_failed = num_failed - 1
		endif
#endif
	endif
end subroutine
! example unary predicates:
pure logical function IsEven(n)
	integer, intent(in) :: n

	IsEven = (mod(n,2) == 0)
end function

pure logical function IsOdd(n)
	integer, intent(in) :: n

	IsOdd = (mod(n,2) == 1)
end function
! example comparators:
pure logical function Greater(n,m)
	integer, intent(in) :: n, m

	Greater = (n > m)
end function

logical function LastDigitMatches(n,m)
	integer, intent(in) :: n, m

	LastDigitMatches = (mod(n,10) == mod(m,10))
end function
! example unary subroutines:
subroutine Square(n)
	integer, intent(inout) :: n

	n = n**2
end subroutine
! helper methods:
integer function RandomInt() result(i)
	real :: r

	call random_number(r)
	i = floor(1e6*r)
end function

real function RandomReal() result(r)
	call random_number(r)
	r = 20.0*r-10.0
end function

integer function FortyTwo()
	FortyTwo = 42
end function
end module
