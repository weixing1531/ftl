#include "ftlTestTools.inc"
module ftlListTestsModule
  use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
  use ftlTestToolsModule
	use ftlListMod
	implicit none

	private
	public :: ftlListTests
  
  type,public::Point
    real::x=0.0
    real::y=0.0
  end type
  
  type,public,extends(ftlList)::NewList
  contains
    procedure,public::WriteList
  end type
contains

!打印链表所有元素值
recursive subroutine WriteList(self, unit)
    class(NewList), intent(in) :: self
    integer, intent(in), optional :: unit !输出文件号 默认为屏幕
    character(len=64) :: string
    type(Iterator) :: it

    integer :: lunit
    integer :: i

    lunit = OUTPUT_UNIT !默认输出为屏幕
    if(present(unit)) lunit = unit

    write(lunit,'(a)',advance='no') '[' !不换行
    
    it=self%Begin() !首元素位置
    i=0 !节点计数
    
    do while(it/=self%End()) !不是尾节点
      i=i+1
      
			select type(v_p => it%value)
        type is(Point)
          write(lunit,'(a,g0,a,g0,a)',advance='no') "(",v_p%x,",",v_p%y,")"
        type is(integer)
          write(lunit,'(i0)',advance='no') v_p
        type is(real) !fmt=*与advance='no'不能同时出现
          write(string,fmt=*) v_p !浮点数转换为字符串
          write(lunit,'(a)',advance='no') trim(adjustl(string)) !删除首尾空格
        type is(real(kind(0d0))) !双精度
          write(string,fmt=*) v_p
          write(lunit,'(a)',advance='no') trim(adjustl(string))
        type is(complex)
          write(string,fmt=*) v_p !复数转换为字符串
          write(lunit,'(a)',advance='no') trim(adjustl(string)) !删除首尾空格
        type is(complex(kind(0d0)))
          write(string,fmt=*) v_p !复数转换为字符串
          write(lunit,'(a)',advance='no') trim(adjustl(string)) !删除首尾空格
        type is(logical)
          if (v_p) then
            write(lunit,'(a)',advance='no') 'true'
          else
            write(lunit,'(a)',advance='no') 'false'
          end if
        type is(character(*))
          write(lunit,'(3a)',advance='no') '"', v_p, '"' !双引号包裹字符串
        type is(ftlList) !元素是另一个链表
          call v_p%WriteList(lunit) !递归调用
        class default !未知类型
          write(lunit,'(a)',advance='no') '*'
      end select
      !尾结点不需要间隔逗号
      if(i<self%Size()) write(lunit,'(a)',advance='no') ',' !元素间隔为逗号
			call it%Inc() !下一节点
		end do
    
    write(lunit,'(a)',advance='no') ']' !不换行
    write(lunit,*) !换行
end subroutine WriteList

subroutine ftlListTests
	write (*,'(A)') 'Running ftlList tests ...'
	call testNewDefault
	call testNewCopyOther
	call testNewFill
	call testNewFromArray
	call testNewFromIteratorPair
	call testAssignments
	call testDelete
	call testArrayFinalizer
	call testInsertSingle
	call testInsertFill
	call testInsertArray
	call testInsertIteratorPair
	call testPushPopBack
	call testPushPopFront
	call testEraseSingle
	call testEraseIteratorPair
	call testSwap
	call testMove
	call testResize
	call testClear
end subroutine

subroutine testNewDefault
	type(ftlList) :: l

	call l%New()
	ASSERT(l%Empty())
	ASSERT(l%Size() == 0)
	ASSERT(Size(l) == 0)
	ASSERT(l%Begin() == l%End())
	ASSERT(.not.(l%Begin() /= l%End()))
end subroutine

subroutine testNewCopyOther
	type(NewList) :: l,o

	call o%New([5,13,41,97,17,10,88])
	call l%New(o)
	ASSERT(.not.l%Empty())
	ASSERT(l%Size() == 7)
	ASSERT(Size(l) == 7)
  
	!ASSERT(l%front == 5)
  call WriteNode(l%front)
	!ASSERT(l%back == 88)
  call WriteNode(l%back)
	ASSERT(.not.associated(o%front,l%front))
	ASSERT(.not.associated(o%back,l%back))
  call o%PushBack((0,0))
  call o%PushBack(Point(1,1))
  call o%WriteList()
end subroutine

subroutine testNewFill
	type(ftlList) :: l

	call l%New(5, 72)
	ASSERT(.not.l%Empty())
	ASSERT(l%Size() == 5)
	ASSERT(Size(l) == 5)
	!ASSERT(l%front == 72)
  call WriteNode(l%front)
	!ASSERT(l%back == 72)
  call WriteNode(l%back)
end subroutine

subroutine testNewFromArray
	type(ftlList) :: l

	call l%New([5,13,41,97,17,10,88])
	ASSERT(.not.l%Empty())
	ASSERT(l%Size() == 7)
	ASSERT(Size(l) == 7)
	!ASSERT(l%front == 5)
  call WriteNode(l%front)
	!ASSERT(l%back == 88)
  call WriteNode(l%back)
end subroutine

subroutine testNewFromIteratorPair
	type(ftlList) :: l, o

	call o%New([5,13,41,97,17,10,88])
	call l%New(o%Begin(), o%End())
	ASSERT(.not.l%Empty())
	ASSERT(l%Size() == 7)
	ASSERT(Size(l) == 7)
	!ASSERT(l%front == 5)
  call WriteNode(l%front)
	!ASSERT(l%back == 88)
  call WriteNode(l%back)
	ASSERT(.not.associated(o%front,l%front))
	ASSERT(.not.associated(o%back,l%back))
end subroutine

subroutine testAssignments
	type(ftlList) :: l, o
	type(Iterator) :: it

	call o%New([5,13,41,97,17,10,88])
	l = o
	ASSERT(.not.l%Empty())
	ASSERT(l%Size() == 7)
	!ASSERT(l%front == 5)
  call WriteNode(l%front)
	!ASSERT(l%back == 88)
  call WriteNode(l%back)
	ASSERT(.not.associated(o%front,l%front))
	ASSERT(.not.associated(o%back,l%back))
	it = l%Begin()
	!ASSERT(it%value == 5)
  call WriteNode(it%value)
	call it%Inc()
	!ASSERT(it%value == 13)
  call WriteNode(it%value)
	call it%Inc()
	!ASSERT(it%value == 41)
  call WriteNode(it%value)
	call it%Inc()
	!ASSERT(it%value == 97)
  call WriteNode(it%value)
	call it%Inc()
	!ASSERT(it%value == 17)
  call WriteNode(it%value)
	call it%Inc()
	!ASSERT(it%value == 10)
  call WriteNode(it%value)
	call it%Inc()
	!ASSERT(it%value == 88)
  call WriteNode(it%value)
	call it%Inc()
	ASSERT(it == l%End())
	l = [23,446,864,3]
	ASSERT(l%Size() == 4)
	!ASSERT(l%front == 23)
  call WriteNode(l%front)
	!ASSERT(l%back == 3)
  call WriteNode(l%back)
end subroutine

subroutine testDelete
	type(ftlList) :: l, uninit

	call uninit%Delete() ! should not crash
	call l%New([5,13,41,97,17,10,88])
	call l%Delete()
	ASSERT(size(l) == 0)
	ASSERT(l%Empty())
	ASSERT(.not.associated(l%front))
	ASSERT(.not.associated(l%back))
end subroutine

subroutine testArrayFinalizer
	type(ftlList), allocatable :: l(:)

	allocate(l(3))
	l(1) = [1,2,3,4,5]
	l(2) = [6,7,8,9]
	l(3) = [10,42]
	! array finalizer should be called here. check that this doesn't leak with 'make memcheck'
end subroutine

subroutine testInsertSingle
	type(ftlList) :: l
	type(Iterator) :: it

	call l%New([23,1,6])
	it = l%Begin()
	!ASSERT(it%value == 23)
  call WriteNode(it%value)
	call it%Inc()
	!ASSERT(it%value == 1)
  call WriteNode(it%value)
	call l%Insert(it, 42)
	!ASSERT(it%value == 1)
  call WriteNode(it%value)
	call it%Dec()
	!ASSERT(it%value == 42)
  call WriteNode(it%value)
	call it%Dec()
	!ASSERT(it%value == 23)
  call WriteNode(it%value)
end subroutine

subroutine testInsertFill
	type(ftlList) :: l
	type(Iterator) :: it

	call l%New([23,1,6])
	it = l%Begin()
	!ASSERT(it%value == 23)
  call WriteNode(it%value)
	call it%Inc()
	!ASSERT(it%value == 1)
  call WriteNode(it%value)
	call l%Insert(it, 2, 42)
	ASSERT(l%Size() == 5)
	!ASSERT(it%value == 1)
  call WriteNode(it%value)
	call it%Dec()
	!ASSERT(it%value == 42)
  call WriteNode(it%value)
	call it%Dec()
	!ASSERT(it%value == 42)
  call WriteNode(it%value)
	call it%Dec()
	!ASSERT(it%value == 23)
  call WriteNode(it%value)
	call l%New()
	ASSERT(l%Empty())
	call l%Insert(l%End(), 2, 42)
	ASSERT(l%Size() == 2)
	!ASSERT(l%front == 42)
  call WriteNode(l%front)
	!ASSERT(l%back == 42)
  call WriteNode(l%back)
end subroutine

subroutine testInsertArray
	type(ftlList) :: l
	type(Iterator) :: it

	call l%New([23,1,6])
	it = l%Begin()
	!ASSERT(it%value == 23)
  call WriteNode(it%value)
	call it%Inc()
	!ASSERT(it%value == 1)
  call WriteNode(it%value)
	call l%Insert(it, [26,11,89])
	!ASSERT(it%value == 1)
  call WriteNode(it%value)
	call it%Dec()
	!ASSERT(it%value == 89)
  call WriteNode(it%value)
	call it%Dec()
	!ASSERT(it%value == 11)
  call WriteNode(it%value)
	call it%Dec()
	!ASSERT(it%value == 26)
  call WriteNode(it%value)
	call it%Dec()
	!ASSERT(it%value == 23)
  call WriteNode(it%value)
	call l%New()
	ASSERT(l%Empty())
	call l%Insert(l%End(), [4,5,6,7])
	ASSERT(l%Size() == 4)
	!ASSERT(l%front == 4)
  call WriteNode(l%front)
	!ASSERT(l%back == 7)
  call WriteNode(l%back)
end subroutine

subroutine testInsertIteratorPair
	type(ftlList) :: l, o
	type(Iterator) :: it

	call o%New([2,3,4])
	call l%New([1,5])
	it = l%Begin()
	call it%Inc()
	!ASSERT(it%value == 5)
  call WriteNode(it%value)
	call l%Insert(it,o%Begin(),o%End())
	ASSERT(l%Size() == 5)
	!ASSERT(l%front == 1)
  call WriteNode(l%front)
	!ASSERT(l%back == 5)
  call WriteNode(l%back)
	it = l%Begin()
	!ASSERT(it%value == 1)
  call WriteNode(it%value)
	call it%Inc()
	!ASSERT(it%value == 2)
  call WriteNode(it%value)
	call it%Inc()
	!ASSERT(it%value == 3)
  call WriteNode(it%value)
	call it%Inc()
	!ASSERT(it%value == 4)
  call WriteNode(it%value)
	call it%Inc()
	!ASSERT(it%value == 5)
  call WriteNode(it%value)
	ASSERT(it /= l%End())
	call it%Inc()
	ASSERT(it == l%End())
end subroutine

subroutine testPushPopBack
	type(ftlList) :: l
	integer :: i

	call l%New()
	ASSERT(l%Empty())
	ASSERT(l%Size() == 0)
	call l%PushBack(5)
	ASSERT(.not.l%Empty())
	ASSERT(l%Size() == 1)
	!ASSERT(l%front == 5)
  call WriteNode(l%front)
	!ASSERT(l%back == 5)
  call WriteNode(l%front)
	call l%PushBack(6)
	ASSERT(.not.l%Empty())
	ASSERT(l%Size() == 2)
	!ASSERT(l%front == 5)
  call WriteNode(l%front)
	!ASSERT(l%back == 6)
  call WriteNode(l%front)
	!i = l%PopBack()
	!ASSERT(i == 6)
  call WriteNode(l%PopBack())
	ASSERT(.not.l%Empty())
	ASSERT(l%Size() == 1)
	!ASSERT(l%front == 5)
  call WriteNode(l%front)
	!ASSERT(l%back == 5)
  call WriteNode(l%back)
	!i = l%PopBack()
	!ASSERT(i == 5)
  call WriteNode(l%PopBack())
	ASSERT(l%Empty())
	ASSERT(l%Size() == 0)
end subroutine

subroutine testPushPopFront
	type(ftlList) :: l
	integer :: i

	call l%New()
	ASSERT(l%Empty())
	ASSERT(l%Size() == 0)
	call l%PushFront(5)
	ASSERT(.not.l%Empty())
	ASSERT(l%Size() == 1)
	!ASSERT(l%front == 5)
  call WriteNode(l%front)
	!ASSERT(l%back == 5)
  call WriteNode(l%back)
	call l%PushFront(6)
	ASSERT(.not.l%Empty())
	ASSERT(l%Size() == 2)
	!ASSERT(l%front == 6)
  call WriteNode(l%front)
	!ASSERT(l%back == 5)
  call WriteNode(l%back)
	!i = l%PopFront()
	!ASSERT(i == 6)
  call WriteNode(l%PopFront())
	ASSERT(.not.l%Empty())
	ASSERT(l%Size() == 1)
	!ASSERT(l%front == 5)
  call WriteNode(l%front)
	!ASSERT(l%back == 5)
  call WriteNode(l%back)
	!i = l%PopFront()
	!ASSERT(i == 5)
  call WriteNode(l%PopFront())
	ASSERT(l%Empty())
	ASSERT(l%Size() == 0)
end subroutine

subroutine testEraseSingle
	type(ftlList) :: l
	type(Iterator) :: it

	call l%New([4,5,6,7])
	it = l%Begin()
	call it%Inc()
	call it%Inc()
	!ASSERT(it%value == 6)
  call WriteNode(it%value)
	call l%Erase(it)
	ASSERT(l%Size() == 3)
	it = l%End()
	call it%Dec()
	!ASSERT(it%value == 7)
  call WriteNode(it%value)
	call it%Dec()
	!ASSERT(it%value == 5)
  call WriteNode(it%value)
	call it%Dec()
	!ASSERT(it%value == 4)
  call WriteNode(it%value)
	ASSERT(it == l%Begin())
	call l%Erase(l%Begin())
	ASSERT(l%Size() == 2)
	!ASSERT(l%front == 5)
  call WriteNode(l%front)
end subroutine

subroutine testEraseIteratorPair
	type(ftlList) :: l
	type(Iterator) :: it1, it2

	call l%New([12,23,34,45,56,76])
	it1 = Begin(l)
	call it1%Inc()
	call it1%Inc()
	!ASSERT(it1%value == 34)
  call WriteNode(it1%value)
	it2 = it1
	call it2%Inc()
	call it2%Inc()
	!ASSERT(it2%value == 56)
  call WriteNode(it2%value)
	call l%Erase(it1, it2)
	ASSERT(l%Size() == 4)
	!ASSERT(it2%value == 56)
  call WriteNode(it2%value)
	call it2%Inc()
	call it2%Inc()
	ASSERT(it2 == l%End())
	call l%New([12,23,34,45,56,76])
	call l%Erase(l%Begin(),l%End())
	ASSERT(l%Empty())
end subroutine

subroutine testSwap
	type(ftlList) :: l, o
	type(Iterator) :: it

	l = [4,7,813,5]
	o = [5,9,6]
	call ftlSwap(l,o)
	ASSERT(l%Size() == 3)
	!ASSERT(l%front == 5)
  call WriteNode(l%front)
	!ASSERT(l%back == 6)
  call WriteNode(l%back)
	it = l%Begin()
	!ASSERT(it%value == 5)
  call WriteNode(it%value)
	call it%Inc()
	!ASSERT(it%value == 9)
  call WriteNode(it%value)
	call it%Inc()
	!ASSERT(it%value == 6)
  call WriteNode(it%value)
	call it%Inc()
	ASSERT(it == l%End())
	ASSERT(o%Size() == 4)
	!ASSERT(o%front == 4)
  call WriteNode(o%front)
	!ASSERT(o%back == 5)
  call WriteNode(o%back)
	it = o%Begin()
	!ASSERT(it%value == 4)
  call WriteNode(it%value)
	call it%Inc()
	!ASSERT(it%value == 7)
  call WriteNode(it%value)
	call it%Inc()
	!ASSERT(it%value == 813)
  call WriteNode(it%value)
	call it%Inc()
	!ASSERT(it%value == 5)
  call WriteNode(it%value)
	call it%Inc()
	ASSERT(it == o%End())
end subroutine

subroutine testMove
	type(ftlList) :: src, dest, uninit
	type(Iterator) :: it

	src = [4,7,813,5]
	dest = [5,9,6]
	call ftlMove(src,dest)
	ASSERT(dest%Size() == 4)
	!ASSERT(dest%front == 4)
  call WriteNode(dest%front)
	!ASSERT(dest%back == 5)
  call WriteNode(dest%back)
	it = dest%Begin()
	!ASSERT(it%value == 4)
  call WriteNode(it%value)
	call it%Inc()
	!ASSERT(it%value == 7)
  call WriteNode(it%value)
	call it%Inc()
	!ASSERT(it%value == 813)
  call WriteNode(it%value)
	call it%Inc()
	!ASSERT(it%value == 5)
  call WriteNode(it%value)
	call it%Inc()
	ASSERT(it == dest%End())
	call ftlMove(uninit,dest)
	ASSERT(dest%Size() == 0)
	ASSERT(dest%Empty())
	ASSERT(.not.associated(dest%front))
	ASSERT(.not.associated(dest%back))
end subroutine

subroutine testResize
	type(ftlList) :: l
	type(Iterator) :: it

	call l%New([5,-2,3,66,0])
	ASSERT(l%Size() == 5)
	!ASSERT(l%back == 0)
  call WriteNode(l%back)
	call l%Resize(4)
	ASSERT(l%Size() == 4)
	!ASSERT(l%back == 66)
  call WriteNode(l%back)
	call l%Resize(6)
	it = l%End()
	call it%Dec()
	call it%Dec()
	call it%Dec()
	!ASSERT(it%value == 66)
  call WriteNode(it%value)
end subroutine

subroutine testClear
	type(ftlList) :: l

	call l%New([4,5,7,8])
	call l%Clear()
	ASSERT(l%Empty())
	ASSERT(l%Size() == 0)
	ASSERT(l%Begin() == l%End())
end subroutine

end module
