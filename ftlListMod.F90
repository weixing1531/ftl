#include "ftlMacros.inc"
module ftlListMod
  use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
	implicit none
	!模块对外接口包括Begin,End,Size,ftlSwap,ftlMove,operator(==),operator(/=)
	private
	! ====== Type of the ftlList container itself ====================================================================================
	type :: ListNode !父类 双向链表节点  哨兵节点或数据节点
		class(ListNode), pointer :: prev => null() !前指针 类指针 可指向父类ListNode及其子类
		class(ListNode), pointer :: next => null() !后指针 类指针
	end type

	type, extends(ListNode) :: DataNode !双向链表节点的子类 新增数据节点 在子程序InsertNodeBefore、InsertNodeAfter中使用
		class(*), allocatable :: data
	end type
	!环状双向链表的节点排列顺序为：哨兵节点－头节点（第一个元素）－...－尾节点（最后一个元素）－哨兵节点－...
	type, public :: ftlList !环状双向链表
		private

		integer :: psize = 0 !链表的元素个数 私有变量
		type(ListNode) :: sentinel !父类 哨兵节点（不保存数据） 私有变量
		class(*), pointer, public :: front => null() !头节点元素数值 公有变量 应当设为protected
		class(*), pointer, public :: back => null()  !尾节点元素数值 公有变量 应当设为protected
	contains
		private
		!链表类对外接口：New,Delete,Begin,End,Size,Empty,=,Insert,Erase,PushFront,PopFront,PushBack,PopBack,Resize,Clear
		procedure :: NewDefault
		procedure :: NewCopyOther
		procedure :: NewFill
		procedure :: NewFromArray
		procedure :: NewFromIteratorPair
		generic , public :: New => NewDefault, NewCopyOther, NewFill, NewFromArray, NewFromIteratorPair !构造函数

		procedure, public :: Delete !消毁链表
		final :: Finalizer !析构函数

		procedure, public :: Begin => BeginList !返回迭代器 链表头节点位置 p%Begin()
		procedure, public :: End => EndList !返回迭代器 链表尾节点位置+1 p%End()

		procedure :: SizeList
		generic , public :: Size => SizeList !链表元素个数 p%Size()

		procedure, public :: Empty !是否链表为空

		procedure :: AssignOther
		procedure :: AssignArray
		generic , public :: assignment(=) => AssignOther, AssignArray !链表赋值号重载
		procedure :: InsertSingle
		procedure :: InsertFill
		procedure :: InsertArray
		procedure :: InsertIteratorPair
		generic , public :: Insert => InsertSingle, InsertFill, InsertArray, InsertIteratorPair !插入链表元素

		procedure :: EraseSingle
		procedure :: EraseIteratorPair
		generic , public :: Erase => EraseSingle, EraseIteratorPair !删除链表元素

		procedure, public :: PushFront !链表头部压入新元素
		procedure, public :: PopFront  !链表头部压出元素
		procedure, public :: PushBack !链表尾部压入新元素
		procedure, public :: PopBack  !链表尾部压出元素

		procedure, public :: Resize !调整链表元素个数

		procedure, public :: Clear !清除链表所有节点
    procedure, public :: WriteList !打印链表 增加

		procedure :: FixValuePtrs !确定链表的头值与尾值
	end type
	! 不绑定也能使用
	public :: Begin !Begin(self) 返回迭代器
	interface Begin
		module procedure BeginList
	end interface
	! 不绑定也能使用
	public :: End !End(self) 返回迭代器
	interface End
		module procedure EndList
	end interface
	! 不绑定也能使用
	public :: Size !Size(self)
	interface Size
		module procedure SizeList
	end interface
	! 交换链表
	public :: ftlSwap
	interface ftlSwap
		module procedure SwapList
	end interface
	! 移动链表
	public :: ftlMove
	interface ftlMove
		module procedure ftlMoveList
	end interface
	! ====== Type of an iterator over a ftlList container ============================================================================
	type, public :: Iterator !迭代器：节点+数据  相当于节点指针
		private

		class(ListNode) , pointer :: node => null() !类指针  可指向父类ListNode及其子类 即哨兵节点或数据节点
		class(*), pointer, public :: value => null() !节点元素数值 公有变量 应当设为protected
	contains
		private
		!迭代器类对外接口：New,Inc,Dec
		procedure :: NewItDefault
		procedure :: NewItCopyOther
		generic , public :: New => NewItDefault, NewItCopyOther !构造函数

		procedure, public :: Inc !下一个节点
		procedure, public :: Dec !上一个节点
	end type

	public :: operator(==) !迭代器相等
	interface operator(==)
		module procedure EqualOther
	end interface

	public :: operator(/=) !迭代器不相等
	interface operator(/=)
		module procedure UnequalOther
	end interface
  
  public :: WriteNode !打印节点 增加
contains
! ====== Implementation of ftlList methods =======================================================================================
subroutine NewDefault(self)
	class(ftlList), intent(out), target :: self
	!链表初始化只有哨兵节点
	self%sentinel%next => self%sentinel !后指针指向哨兵节点自身
	self%sentinel%prev => self%sentinel !前指针指向哨兵节点自身
end subroutine
!
impure elemental subroutine NewCopyOther(self, other)
	class(ftlList), intent(inout) :: self
	!type(ftlList), intent(in) :: other
  class(ftlList), intent(in) :: other !改动

	type(Iterator) :: it !迭代器

	call self%New()

	it = other%Begin() !头节点

	do while (it /= other%End())
		call self%PushBack(it%value) !尾部压入
		call it%Inc() !下一个节点
	enddo
end subroutine
!n个元素的初始值均设为val
subroutine NewFill(self, n, val)
	class(ftlList), intent(inout) :: self
	integer , intent(in) :: n
	class(*) , intent(in), optional :: val

	integer :: i

	call self%New()

	if (present(val)) then
		do i = 1, n
			call self%PushBack(val) !尾部压入
		enddo
	else
		stop 'TODO: Implement ftlList%NewFill without val'
	endif
end subroutine
!array必须为一维数组
subroutine NewFromArray(self, array)
	class(ftlList), intent(inout) :: self
	class(*) , intent(in) :: array(:)
	integer :: i

	call self%New()

	do i = 1, size(array)
		call self%PushBack(array(i)) !尾部压入
	enddo
end subroutine
!由迭代器构造链表
subroutine NewFromIteratorPair(self, first, last)
	class(ftlList) , intent(inout) :: self
	type(Iterator), intent(in) :: first !迭代器
	type(Iterator), intent(in) :: last !迭代器

	call self%New()
	call self%Insert(self%Begin(), first, last)
end subroutine
!销毁链表
impure elemental subroutine Delete(self)
	class(ftlList), intent(inout), target :: self
	class(ListNode), pointer :: walker, deletor

	walker => self%sentinel%next !头节点

	do while (associated(walker) .and. .not.associated(walker,self%sentinel))
		deletor => walker
		walker => walker%next
		deallocate(deletor) !删除该节点并释放其内存
	enddo

	self%psize = 0 !元素个数置0
	nullify(self%sentinel%prev) !前指针置空
	nullify(self%sentinel%next) !后指针置空
	nullify(self%front)
	nullify(self%back)
end subroutine
!析构函数
impure elemental subroutine Finalizer(self)
	type(ftlList), intent(inout) :: self

	call self%Delete()
end subroutine
! =============> Iterators:
type(Iterator) function BeginList(self) result(Begin) !迭代器 链表的头节点
	class(ftlList), intent(in), target :: self

	Begin%node => self%sentinel%next !头节点 迭代器也可以指向数据节点(子类DataNode)

	select type (node => Begin%node) !node为别名
		type is (DataNode)
		Begin%value => node%data
	end select
end function

type(Iterator) function EndList(self) result(End) !迭代器 链表的节点结束标志（也是哨兵节点）
	class(ftlList), intent(in), target :: self

	End%node => self%sentinel !类定义中class(ListNode)多态指针的原因 迭代器也可以指向哨兵节点(父类ListNode)
end function
! =============> Capacity:
pure integer function SizeList(self) result(Size) !链表的元素个数
	class(ftlList), intent(in) :: self

	Size = self%psize
end function

pure logical function Empty(self)  !链表是否为空
	class(ftlList), intent(in) :: self

	Empty = (self%psize == 0)
end function
! =============> Modifiers:
! TODO: implement using existing list nodes instead of copy construction
!
impure elemental subroutine AssignOther(self, other) !链表赋值
	class(ftlList), intent(inout) :: self
	type(ftlList), intent(in) :: other

#if defined(NAGFOR)
	! For some reason NAGFOR needs a special implementation here. I suspect that this is a compiler bug. The normal
	! implementation, as in %NewCopyOther doesn't work, because the iterator into other never becomes equal to other%End(),
	! causing an infinite loop until we run out of memory.
	! For some reason this seems to be related to this method being called through the assignment statement, because this doesn't
	! happen if %NewCopyOther is called directly, or %AssignOther is made public and called directly. I don't really understand
	! this, it's either a NAGFOR bug or I'm totally not understanding something about user defined assignment in Fortran ...
	type(Iterator) :: it !迭代器
	integer :: i

	call self%New()
	i = 1
	it = other%Begin()

	do while (i <= other%Size())
		call self%PushBack(it%value)
		i = i + 1
		call it%Inc() !下一个节点
	enddo
#else
	call self%New(other)
#endif
end subroutine
!由一维数组赋值给链表
subroutine AssignArray(self, array)
	class(ftlList), intent(inout) :: self
	class(*) , intent(in) :: array(:)

	call self%New(array)
end subroutine
!链表迭代器position位置之前插入1个元素,其值为val
subroutine InsertSingle(self, position, val)
	class(ftlList) , intent(inout) :: self
	type(Iterator) :: position !迭代器
	class(*) , intent(in) :: val

	call self%InsertFill(position, 1, val)
end subroutine
!链表迭代器position位置之前插入n个元素,其值均为val
subroutine InsertFill(self, position, n, val)
	class(ftlList) , intent(inout) :: self
	type(Iterator) :: position !迭代器
	integer , intent(in) :: n
	class(*) , intent(in) :: val
	integer :: i

	do i = 1, n
		call InsertNodeBefore(position%node, val) !前插 InsertNodeBefore第一个参数是多态节点类指针
	enddo

	self%psize = self%psize + n !链表元素个数改变
	call self%FixValuePtrs()  !确定链表的头值与尾值
end subroutine
!链表迭代器position位置之前插入数组array
subroutine InsertArray(self, position, array)
	class(ftlList) , intent(inout) :: self
	type(Iterator) :: position
	class(*) , intent(in) :: array(:)
	integer :: i

	do i = 1, size(array)
		call InsertNodeBefore(position%node, array(i)) !前插 InsertNodeBefore第一个参数是多态节点类指针
	enddo

	self%psize = self%psize + size(array) !链表元素个数改变
	call self%FixValuePtrs()  !确定链表的头值与尾值
end subroutine
!链表迭代器position位置之前插入迭代器 从first到last-1
subroutine InsertIteratorPair(self, position, first, last)
	class(ftlList) , intent(inout) :: self
	type(Iterator) :: position
	type(Iterator), intent(in) :: first
	type(Iterator), intent(in) :: last
	type(Iterator) :: it

	it = first

	do while (it /= last)
		call InsertNodeBefore(position%node, it%value) !前插 InsertNodeBefore第一个参数是多态节点类指针
		self%psize = self%psize + 1 !链表元素个数改变
		call it%Inc() !下一个节点
	enddo

	call self%FixValuePtrs()  !确定链表的头值与尾值
end subroutine
!头部压入
subroutine PushFront(self, val)
	class(ftlList), intent(inout), target :: self
	class(*) , intent(in) :: val

	call InsertNodeBefore(self%sentinel%next, val) !前插 InsertNodeBefore第一个参数是多态节点类指针
	self%psize = self%psize + 1 !链表元素个数改变
	call self%FixValuePtrs()  !确定链表的头值与尾值
end subroutine
!头部压出
function PopFront(self) result(Res)
	class(ftlList), intent(inout), target :: self
	class(ListNode), pointer :: oldfirst
  class(*), allocatable :: Res

	oldfirst => self%sentinel%next !头节点（第一个元素对应的节点）
#ifdef FTL_TEMPLATE_TYPE_PROVIDES_FTLMOVE
	call ftlMove(self%front, Res) !适用于派生类型
#else
	Res = self%front !适用于自带类型
#endif
	self%psize = self%psize - 1 !链表元素个数改变
	call UnlinkNode(oldfirst) !环状双向链表中删除该节点
	deallocate(oldfirst) !释放该节点内存
	call self%FixValuePtrs()  !确定链表的头值与尾值
end function
!尾部压入
subroutine PushBack(self, val)
	class(ftlList), intent(inout) :: self
	class(*) , intent(in) :: val

	call InsertNodeAfter(self%sentinel%prev, val) !后插 InsertNodeAfter第一个参数是多态节点类指针
	self%psize = self%psize + 1 !链表元素个数改变
	call self%FixValuePtrs()  !确定链表的头值与尾值
end subroutine
!尾部压出
function PopBack(self) result(Res)
	class(ftlList), intent(inout), target :: self
	class(ListNode), pointer :: oldlast
  class(*), allocatable :: Res

	oldlast => self%sentinel%prev !尾节点（最后一个元素对应的节点）
#ifdef FTL_TEMPLATE_TYPE_PROVIDES_FTLMOVE
	call ftlMove(self%back, Res) !适用于派生类型
#else
	Res = self%back !适用于自带类型
#endif
	self%psize = self%psize - 1 !链表元素个数改变
	call UnlinkNode(oldlast) !环状双向链表中删除该节点
	deallocate(oldlast) !释放该节点内存
	call self%FixValuePtrs()  !确定链表的头值与尾值
end function
!删除迭代器position节点
subroutine EraseSingle(self, position)
	class(ftlList) , intent(inout) :: self
	type(Iterator) :: position

	call UnlinkNode(position%node) !环状双向链表中删除该节点
	deallocate(position%node) !释放该节点内存
	self%psize = self%psize - 1 !链表元素个数改变
	call self%FixValuePtrs()  !确定链表的头值与尾值
end subroutine
!
subroutine EraseIteratorPair(self, first, last) !从first到last-1删除元素
	class(ftlList) , intent(inout) :: self
	type(Iterator) :: first !注意调用结束后first已改变
	type(Iterator), intent(in) :: last
	type(Iterator) :: deletor

	associate(walker => first) !walker是first的别名
		do while (walker /= last)
			deletor = walker
			call walker%Inc() !下一个节点
			call self%EraseSingle(deletor) !删除deletor节点
		enddo
	end associate
end subroutine
!交换链表
subroutine SwapList(self, other)
	type(ftlList), intent(inout), target :: self
	type(ftlList), intent(inout), target :: other
	integer :: tmpSize
	type(ListNode) :: tmpNode

	! fix pointers from data nodes to the sentinels

	self%sentinel%prev%next => other%sentinel
	self%sentinel%next%prev => other%sentinel

	other%sentinel%prev%next => self%sentinel
	other%sentinel%next%prev => self%sentinel

	! exchange sentinels themselves
	!交换哨兵节点和元素个数
	tmpNode = self%sentinel
	tmpSize = self%psize

	self%sentinel = other%sentinel
	self%psize = other%psize

	other%sentinel = tmpNode
	other%psize = tmpSize

	! fix front/back pointers for both lists

	call self%FixValuePtrs()  !确定链表的头值与尾值
	call other%FixValuePtrs()  !确定链表的头值与尾值
end subroutine

subroutine Resize(self, n, val)
	class(ftlList), intent(inout) :: self
	integer , intent(in) :: n
	class(*) , intent(in) , optional :: val
	type(Iterator) :: it
	integer :: i

	if (n == self%psize) then
		return !子程序不进行任何操作
	else if (n < self%psize) then
		it = self%Begin() !头节点

		do i = 2, n
			call it%Inc() !下一个节点
		enddo
		!此时位置为n
		call it%Inc() !下一个节点 此时位置为n+1
		call self%Erase(it,self%End()) !删除多余尾巴
	else ! n > self%psize
		do i = 1, n - self%psize
			call InsertNodeAfter(self%sentinel%prev, val) !后插  （尾节点之后）补充不足部分
		enddo
	endif

	self%psize = n !链表元素个数改变
	call self%FixValuePtrs()  !确定链表的头值与尾值
end subroutine
!清除所有节点
subroutine Clear(self)
	class(ftlList), intent(inout) :: self

	call self%New()
end subroutine
! =============> FTL methods:
subroutine ftlMoveList(src, dest) !src旧毁,dest新生
	type(ftlList), intent(inout) :: src
	type(ftlList), intent(out) , target :: dest

	dest%psize = src%psize
	dest%sentinel = src%sentinel
	if (associated(dest%sentinel%next)) dest%sentinel%next%prev => dest%sentinel
	if (associated(dest%sentinel%prev)) dest%sentinel%prev%next => dest%sentinel
	call dest%FixValuePtrs()  !确定链表的头值与尾值
	nullify(src%sentinel%prev)
	nullify(src%sentinel%next)
	nullify(src%front)
	nullify(src%back)
	src%psize = 0
end subroutine
! =============> Internal methods:
subroutine InsertNodeBefore(beforenode, val) !在beforenode位置前插入元素
	class(ListNode), pointer, intent(inout) :: beforenode !注意是节点类多态指针
	class(*) , intent(in) , optional :: val
	class(ListNode), pointer :: oldprev, newprev

	oldprev => beforenode%prev !上一个节点
	allocate(DataNode::beforenode%prev) !新数据节点分配子类内存
	newprev => beforenode%prev
	newprev%next => beforenode
	newprev%prev => oldprev
	oldprev%next => newprev

	if (present(val)) then
		select type (newprev)
			type is (DataNode) ! always true
			newprev%data = val
		end select
	endif
end subroutine

subroutine InsertNodeAfter(afternode, val) !在afternode位置后插入元素
	class(ListNode), pointer, intent(inout) :: afternode !注意是节点类多态指针
	class(*) , intent(in) , optional :: val
	class(ListNode), pointer :: oldnext, newnext

	oldnext => afternode%next !下一个节点
	allocate(DataNode::afternode%next) !新数据节点分配子类内存
	newnext => afternode%next
	newnext%prev => afternode
	newnext%next => oldnext
	oldnext%prev => newnext

	if (present(val)) then
		select type (newnext)
			type is (DataNode) ! always true
			newnext%data = val
		end select
	endif
end subroutine
!环状双向链表中删除该节点 但不释放内存
subroutine UnlinkNode(node)
	class(ListNode), intent(inout) :: node !注意是节点类多态指针

	node%next%prev => node%prev
	node%prev%next => node%next
end subroutine
!确定链表的头值与尾值
subroutine FixValuePtrs(self)
	class(ftlList), intent(inout) :: self

	if (self%psize == 0) then
		nullify(self%front,self%back)
	else
		select type (first => self%sentinel%next) !头节点 数据节点
			type is (DataNode)
			self%front => first%data
		end select

		select type (last => self%sentinel%prev) !尾节点 数据节点
			type is (DataNode)
			self%back => last%data
		end select
	endif
end subroutine
! ====== Implementation of ftlListIterator methods ===============================================================================
subroutine NewItDefault(self)
	class(Iterator), intent(out) :: self
	! Nothing to do here: intent(out) already resets everything
end subroutine
!
subroutine NewItCopyOther(self, other)
	class(Iterator), intent(out) :: self
	class(Iterator), intent(in) :: other

	self%node => other%node

	select type (node => self%node) !node为别名
		type is (DataNode)
		self%value => node%data
	end select
end subroutine
! =============> Arithmetic operations:
subroutine Inc(self) !下一个节点
	class(Iterator), intent(inout) :: self

	self%node => self%node%next !下一个节点

	select type (node => self%node) !node为别名
		type is (DataNode)
		self%value => node%data
	end select
end subroutine
!
subroutine Dec(self) !上一个节点
	class(Iterator), intent(inout) :: self

	self%node => self%node%prev !上一个节点

	select type (node => self%node) !node为别名
		type is (DataNode)
		self%value => node%data
	end select
end subroutine
! =============> Logical operations:
pure logical function EqualOther(self, other)
	class(Iterator), intent(in) :: self
	class(Iterator), intent(in) :: other

	EqualOther = associated(self%node,other%node)
end function
!
pure logical function UnequalOther(self, other)
	class(Iterator), intent(in) :: self
	class(Iterator), intent(in) :: other

	UnequalOther = .not.associated(self%node,other%node)
end function

!打印链表所有元素值
recursive subroutine WriteList(self, unit)
    class(ftlList), intent(in) :: self
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
      if(i<self%psize) write(lunit,'(a)',advance='no') ',' !元素间隔为逗号
			call it%Inc() !下一节点
		end do
    
    write(lunit,'(a)',advance='no') ']' !不换行
    write(lunit,*) !换行
end subroutine WriteList

!打印元素值 模块方法
subroutine WriteNode(val, unit, name)
    class(*), intent(in) :: val
    integer, intent(in), optional :: unit !输出文件号 默认为屏?
    character(*), intent(in), optional :: name !元素名称

    character(len=64) :: string
    integer :: lunit

    lunit = OUTPUT_UNIT !默认输出为屏幕
    if(present(unit)) lunit = unit

    if(present(name))then
      write(lunit,'(a)',advance='no') name // '=' !不换行
    else
      write(lunit,'(a)',advance='no') "Node" // '=' !不换行
    end if
    !无限多态引用必须用select type
    select type(v_p => val)
    type is(integer)
      write(lunit,'(i0)',advance='no') v_p
    type is(real) !fmt=*与advance='no'不能同时出现
      write(string,fmt=*) v_p
      write(lunit,'(a)',advance='no') trim(adjustl(string))
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
      write(lunit,'(3a)',advance='no') '"', v_p, '"'
    type is(ftlList) !元素是另一个链表
      call v_p%WriteList(lunit)
    class default !未知类型
      write(lunit,'(a)',advance='no') '*'
    end select
    write(lunit,*) !换行

end subroutine WriteNode
end module
