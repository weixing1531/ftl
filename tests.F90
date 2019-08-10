#include "ftlTestTools.inc"
program tests
  use ftlTestToolsModule
	use ftlListTestsModule

#if !defined(NAGFOR)
	! dummy assertion to test that assertions themselves work ...
	ASSERT(.false.)
#endif

	call ftlListTests

	write (*,'(A,I0,A,I0)') 'Failed assertions: ',num_failed,'/',num_asserts

	if (num_failed > 0) then
		write (*,'(A)') 'TEST FAILED'
		error stop
	else
		write (*,'(A)') 'TEST PASSED'
	endif
end program
