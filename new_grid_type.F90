module set_precision
  use iso_fortran_env, only : real64, int32, int64
  implicit none
  private
  public :: dp, i4, i8
  integer, parameter :: dp  = real64
  integer, parameter :: i4  = int32
  integer, parameter :: i8  = int64
end module set_precision

module set_constants
  use set_precision, only : dp
  implicit none
  private
  public :: zero, one, two, three, four, ten
  public :: half, third, fourth
  public :: pi, large, near_zero
  public :: max_text_line_length
  real(dp), parameter :: zero      = 0.0_dp
  real(dp), parameter :: one       = 1.0_dp
  real(dp), parameter :: two       = 2.0_dp
  real(dp), parameter :: three     = 3.0_dp
  real(dp), parameter :: four      = 4.0_dp
  real(dp), parameter :: ten       = 10.0_dp
  real(dp), parameter :: third     = one / three
  real(dp), parameter :: fourth    = 0.25_dp
  real(dp), parameter :: half      = 0.50_dp
  real(dp), parameter :: large  = huge(one)
  real(dp), parameter :: pi     = acos(-one)
  real(dp), parameter :: near_zero = epsilon(one)
  integer,  parameter :: max_text_line_length = 1024
end module set_constants

module combinatorics
  implicit none
  private
  public :: nchoosek
  public :: n_hyper_cube_polytopes
  public :: rand_int_in_range, unique_rand_ints_1D
contains

  function unique_rand_ints_1D(lo,hi,sz) result(num)
    integer, intent(in)    :: lo, hi, sz
    integer, dimension(sz) :: num
    integer :: i
    num(1) = rand_int_in_range(lo,hi)
    do i = 2,sz
      do
        num(i) = rand_int_in_range(lo,hi)
        if ( all( num(i)/=num(1:i-1) ) ) exit
      end do
    end do
  end function unique_rand_ints_1D

  function rand_int_in_range(lo,hi) result(num)
    use set_precision, only : dp
    integer, intent(in) :: lo, hi
    integer             :: num
    real(dp) :: harvest
    call random_number(harvest)
    num = nint( harvest*real(hi-lo,dp) + real(lo,dp) )
  end function rand_int_in_range

  pure elemental function nchoosek( n, k ) result( c )
    integer, intent(in) :: n, k
    integer             :: c
    integer :: i
    c = 0
    if (k>n) return

    c = 1
    do i = 1, min(n-k,k)
      c = c * ( n - (i-1) )
      c = c / i
    end do
  end function nchoosek

  pure elemental function n_hyper_cube_polytopes(n_dim,i) result(c)
    integer, intent(in) :: n_dim, i
    integer             :: c
    integer, parameter  :: max_dim = 10
    c = -1
    if (n_dim>10) return
    c = 0
    if ( i>n_dim ) return
    c = 2**(n_dim-i) * nchoosek(n_dim,i)
  end function n_hyper_cube_polytopes

end module combinatorics

module string_stuff
  implicit none
  private
  public :: generate_newline_string
  public :: progress_line, iteration_line
contains
  subroutine generate_newline_string(strings,out_fmt)
    character(*), dimension(:), intent(in)  :: strings
    character(*)              , intent(out) :: out_fmt
    integer :: j, sz
    sz = size(strings)
    out_fmt = '('//trim(strings(1))
    do j=2,sz
        out_fmt=trim(out_fmt)//',/,'//trim(strings(j))
    end do
    out_fmt=trim(out_fmt)//')'
  end subroutine generate_newline_string

  subroutine update_current_line(string)
    use iso_fortran_env, only : std_out => output_unit
    character(*), intent(in) :: string
    write(std_out,'(A)',advance='no') string
    flush(std_out)
  end subroutine update_current_line

  subroutine progress_line(string,n,n_total)
    use set_precision, only : dp
    character(*), intent(in) :: string
    integer,      intent(in) :: n, n_total
    character(*), parameter :: fmt = '(A,I0,A,I0,A,F5.1,A)'
    character(*), parameter :: carriage_return = achar(13)
    character(len=100) :: out_string
    write(out_string,fmt) string, n,'/',n_total, ' (', real(n,dp)/real(n_total,dp)*100.0_dp, '%)'
    call update_current_line(carriage_return//trim(out_string))
  end subroutine progress_line

  subroutine iteration_line(string,n,residual)
    use set_precision, only : dp
    character(*), intent(in) :: string
    integer,      intent(in) :: n
    real(dp), dimension(:), intent(in) :: residual
    character(*), parameter :: fmt1 = '("(A,I0,",I0,"("" "",ES18.12))")'
    character(*), parameter :: carriage_return = achar(13)
    character(len=100) :: fmt, out_string
    write(fmt,fmt1) size(residual)
    write(out_string,fmt) string, n, residual
    call update_current_line(carriage_return//trim(out_string))
  end subroutine iteration_line
end module string_stuff

module project_inputs
  implicit none
  private
  public :: verbose_level
  integer :: verbose_level = 0
end module project_inputs

module message
  use ISO_FORTRAN_ENV, only : error_unit
  implicit none
  private
  public :: error_message, warning_message
  public :: WARN_ALWAYS, WARN_SOMETIMES, WARN_RARELY
  integer, parameter :: WARN_ALWAYS    = 0
  integer, parameter :: WARN_SOMETIMES = 1
  integer, parameter :: WARN_RARELY    = 2
contains
  function error_message( routine_name, message ) result( err )
    character(*), intent(in) :: routine_name
    character(*), intent(in) :: message
    logical                  :: err
    err = .true.
    write(error_unit,*)
    write(error_unit,*) ' ERROR: In ' // trim(routine_name)
    write(error_unit,*) '   ', trim(message)
    write(error_unit,*) ' Stopping ...'
    call abort
    stop
  end function error_message

  function warning_message( warn_level, routine_name, message ) result( warn )
    use project_inputs,  only : verbose_level
    integer,      intent(in) :: warn_level
    character(*), intent(in) :: routine_name
    character(*), intent(in) :: message
    logical                  :: warn
    warn = .true. ! Setup
    if ( warn_level <= verbose_level ) then ! Print Warning Message
      write(error_unit,*)
      write(error_unit,*) ' WARNING: In ' // trim(routine_name)
      write(error_unit,*) '   ', trim(message)
    end if
  end function warning_message

end module message

module timer_derived_type

  use set_precision, only : dp
  use set_constants, only : zero
  implicit none
  private
  public :: basic_timer_t

  type :: basic_timer_t
    private
    real(dp)         :: time_start   = zero
    real(dp), public :: time_elapsed = zero
  contains
    private
    procedure, public, pass :: tic => timer_tick
    procedure, public, pass :: toc => timer_tock
  end type basic_timer_t

contains

  function get_time()
    integer(kind=8) :: ticks, ticks_per_sec, max_ticks
    real(dp) :: get_time
    call system_clock( count      = ticks,                                     &
                      count_rate = ticks_per_sec,                              &
                      count_max  = max_ticks )
    if ( ticks_per_sec == 0 ) then
      get_time = zero
    else
      get_time = real(ticks,dp) / real(ticks_per_sec,dp)
    end if
  end function get_time

  subroutine timer_tick( this )
    class(basic_timer_t), intent(inout) :: this
    this%time_elapsed = zero
    this%time_start   = get_time()
  end subroutine timer_tick

  function timer_tock( this )
    class(basic_timer_t), intent(in) :: this
    real(dp)                         :: timer_tock
    timer_tock = get_time() - this%time_start
  end function timer_tock

end module timer_derived_type

module quick_sort
  implicit none
  private
  public :: sort
contains
  pure subroutine sort(array,sorted,idx)
    integer, dimension(:),           intent(in) :: array
    integer, dimension(size(array)), optional, intent(out) :: sorted
    integer, dimension(:), optional, intent(inout) :: idx
    integer, dimension(size(array)) :: idx_, sorted_
    integer :: m
    m = size(array)
    sorted_ = array
    if ( present(idx) ) then
      call qsort_1D( m, sorted_, idx )
    else
      call qsort_1D( m, sorted_, idx_ )
    end if
    if ( present(sorted) ) sorted = sorted_
  end subroutine sort

  pure recursive subroutine qsort_1D( m, A, indx )
    integer,               intent(in)    :: m
    integer, dimension(m), intent(inout) :: A
    integer, dimension(m), intent(inout) :: indx
    integer :: iq
    if ( m > 1 ) then
      call partition_1D( m, A, indx, iq )
      call qsort_1D( iq-1  , A(1:iq-1), indx(1:iq-1) )
      call qsort_1D( m-iq+1, A(iq:m)  , indx(iq:m)   )
    end if
  end subroutine qsort_1D

  pure subroutine partition_1D( m, A, indx, marker )
    integer,               intent(in)    :: m
    integer, dimension(m), intent(inout) :: A
    integer, dimension(m), intent(inout) :: indx
    integer,               intent(out)   :: marker
    integer :: i, j
    integer :: temp_indx
    integer :: x, temp_A
    x = A(1)
    i = 0
    j = m+1
    do
      do; j = j-1; if ( A(j) <= x ) exit; end do
      do; i = i+1; if ( A(i) >= x ) exit; end do
      if ( i < j ) then
        temp_A    = A(i);   temp_indx = indx(i)
        A(i)      = A(j);   indx(i)   = indx(j)
        A(j)      = temp_A; indx(j)   = temp_indx
      elseif ( i == j ) then
        marker = i+1; return
      else
        marker = i; return
      end if
    end do
  end subroutine partition_1D
end module quick_sort

module index_conversion
  use set_precision, only : i4, i8
  implicit none
  private
  public :: global2local, local2global

  interface global2local
    module procedure global2local_i8_i8
    module procedure global2local_i8_i8_bnd
    module procedure global2local_i4_i8
    module procedure global2local_i4_i8_bnd
  end interface global2local

  interface local2global
    module procedure local2global_i8_i8
    module procedure local2global_i8_i8_bnd
    module procedure local2global_i4_i8
    module procedure local2global_i4_i8_bnd
  end interface local2global
contains

  pure function local2global_i8_i8(iSub,nSub) result(iG)
    integer(i8), dimension(:), intent(in) :: iSub, nSub
    integer(i8) :: iG
    integer(i8) :: nDims, p, i
    nDims = size(iSub)
    p = 1
    iG = 1
    do i = 1,nDims
        iG = iG + ( iSub(i) - 1 )*p
        p = p*nSub(i)
    end do
  end function local2global_i8_i8

  pure function local2global_i4_i8(iSub,nSub) result(iG)
    integer(i4), dimension(:), intent(in) :: iSub, nSub
    integer(i8) :: iG
    iG = local2global_i8_i8(int(iSub,i8),int(nSub,i8))
  end function local2global_i4_i8

  pure function local2global_i8_i8_bnd(iSub,lo,hi) result(iG)
    integer(i8), dimension(:), intent(in) :: iSub, lo, hi
    integer(i8), dimension(size(iSub)) :: idx, nSub
    integer(i8) :: iG
    idx  = iSub - lo + 1
    nSub = hi - lo + 1
    iG   = local2global_i8_i8(idx,nSub)
  end function local2global_i8_i8_bnd

  pure function local2global_i4_i8_bnd(iSub,lo,hi) result(iG)
    integer(i4), dimension(:), intent(in) :: iSub, lo, hi
    integer(i8) :: iG
    iG = local2global_i8_i8_bnd(int(iSub,i8),int(lo,i8),int(hi,i8))
  end function local2global_i4_i8_bnd


  pure function global2local_i8_i8(iG,nSub) result(iSub)
    integer(i8),               intent(in) :: iG
    integer(i8), dimension(:), intent(in) :: nSub
    integer(i8), dimension(size(nSub)) :: iSub
    integer(i8) :: i, nDims, p, iGtmp, iTmp
    nDims = size(nSub)
    if (nDims==1) then
      iSub(1) = iG
      return
    end if
    p = product(nSub)
    iGtmp = iG
    do i = nDims,1,-1
      p = p/nSub(i)
      iTmp = mod(iGtmp-1,p) + 1
      iSub(i) = (iGtmp-iTmp)/p + 1
      iGtmp = iTmp
    end do
  end function global2local_i8_i8

  pure function global2local_i4_i8(iG,nSub) result(iSub)
    integer(i4),               intent(in) :: iG
    integer(i4), dimension(:), intent(in) :: nSub
    integer(i8), dimension(size(nSub)) :: iSub
    iSub = global2local_i8_i8(int(iG,i8),int(nSub,i8))
  end function global2local_i4_i8

  pure function global2local_i8_i8_bnd(iG,lo,hi) result(iSub)
    integer(i8),               intent(in) :: iG
    integer(i8), dimension(:), intent(in) :: lo, hi
    integer(i8), dimension(size(lo)) :: iSub, nSub
    nSub = hi - lo + 1
    iSub = global2local_i8_i8(iG,nSub)
    iSub = iSub + lo - 1
  end function global2local_i8_i8_bnd

  pure function global2local_i4_i8_bnd(iG,lo,hi) result(iSub)
    integer(i4),               intent(in) :: iG
    integer(i4), dimension(:), intent(in) :: lo, hi
    integer(i8), dimension(size(lo)) :: iSub
    iSub = global2local_i8_i8_bnd(int(iG,i8),int(lo,i8),int(hi,i8))
  end function global2local_i4_i8_bnd

end module index_conversion


module basic_bnd_info
  use set_precision, only : dp, i8
  implicit none
  private
  public :: basic_bnd_info_t

  type basic_bnd_info_t
    integer(i8) :: n_dim, n_c, block_id
    integer(i8) :: glo_idx_lo, glo_idx_hi
    integer(i8), dimension(:),   allocatable :: loc_idx_lo, loc_idx_hi
    real(dp),    dimension(:,:), allocatable :: corner_nodes
    logical :: has_nodes
  contains
    private
    procedure, public, pass :: setup     => setup_basic_bnd_info
    procedure, public, pass :: set_nodes
    procedure, public, pass :: destroy => destroy_basic_bnd_info
  end type basic_bnd_info_t

  interface basic_bnd_info_t
    module procedure :: constructor
  end interface basic_bnd_info_t
contains
  pure elemental subroutine destroy_basic_bnd_info( this )
    class(basic_bnd_info_t), intent(inout) :: this
    if ( allocated(this%loc_idx_lo)   ) deallocate(this%loc_idx_lo  )
    if ( allocated(this%loc_idx_hi)   ) deallocate(this%loc_idx_hi  )
    if ( allocated(this%corner_nodes) ) deallocate(this%corner_nodes)
    this%glo_idx_lo = -1
    this%glo_idx_hi = -1
    this%block_id   = -1
    this%n_dim      = -1
    this%n_c        = -1
  end subroutine destroy_basic_bnd_info

  pure elemental function constructor( n_dim, has_nodes ) result(this)
    use set_constants, only : zero
    integer, intent(in)    :: n_dim
    logical, intent(in)    :: has_nodes
    type(basic_bnd_info_t) :: this
    call this%destroy()
    this%n_dim      = int( n_dim, i8 )
    this%n_c        = 2**(this%n_dim-1)
    this%block_id   = 0
    this%glo_idx_lo = 0
    this%glo_idx_hi = 0
    this%has_nodes  = has_nodes
    allocate( this%loc_idx_lo(n_dim)  )
    allocate( this%loc_idx_hi(n_dim)  )
    this%loc_idx_lo = -1
    this%loc_idx_hi = -1
    if ( has_nodes ) then
      allocate( this%corner_nodes(n_dim,2**n_dim)  )
      this%corner_nodes = zero
    end if
  end function constructor

  pure subroutine setup_basic_bnd_info( this, block_id, glo_idx_lo, glo_idx_hi, loc_idx_lo, loc_idx_hi )
    class(basic_bnd_info_t),   intent(inout) :: this
    integer(i8),               intent(in)    :: block_id, glo_idx_lo, glo_idx_hi
    integer(i8), dimension(:), intent(in)    :: loc_idx_lo, loc_idx_hi
    this%block_id   = block_id
    this%glo_idx_lo = glo_idx_lo
    this%glo_idx_hi = glo_idx_hi
    this%loc_idx_lo = loc_idx_lo(1:this%n_dim)
    this%loc_idx_hi = loc_idx_hi(1:this%n_dim)
  end subroutine setup_basic_bnd_info

  pure subroutine set_nodes( this, nodes )
    class(basic_bnd_info_t),     intent(inout) :: this
    integer(i8), dimension(:,:), intent(in)    :: nodes
    if ( .not. this%has_nodes ) then
      this%has_nodes = .true.
      if ( allocated( this%corner_nodes ) ) deallocate( this%corner_nodes )
      allocate( this%corner_nodes(this%n_dim,this%n_c)  )
    end if
    this%corner_nodes = nodes(1:this%n_dim,1:this%n_c)
  end subroutine set_nodes

end module basic_bnd_info




module block_info_type
  use set_precision,  only : dp, i8
  use basic_bnd_info, only : basic_bnd_info_t
  implicit none
  private
  public :: block_info_t

  type block_info_t
    integer(i8) :: n_dim, n_blocks, n_bnd
    integer(i8), dimension(:),   allocatable :: id, n_off, c_off
    integer(i8), dimension(:,:), allocatable :: sz, ng
    type(basic_bnd_info_t), dimension(:,:), allocatable :: bnd
  contains
    private
    procedure, public, pass :: setup   => setup_block_info_type
    procedure, public, pass :: destroy => destroy_block_info_type
    procedure, public, pass :: n_idx_loc => get_local_node_idx_from_global
    procedure, public, pass :: n_idx_glo => get_global_node_idx_from_local
    procedure, public, pass :: c_idx_loc => get_local_cell_idx_from_global
    procedure, public, pass :: c_idx_glo => get_global_cell_idx_from_local
  end type block_info_t

  interface block_info_t
    module procedure constructor
  end interface block_info_t

contains
  pure elemental subroutine destroy_block_info_type( this )
    class(block_info_t), intent(inout) :: this
    if ( allocated(this%id ) ) deallocate(this%id )
    if ( allocated(this%n_off) ) deallocate(this%n_off)
    if ( allocated(this%c_off) ) deallocate(this%c_off)
    if ( allocated(this%sz ) ) deallocate(this%sz )
    if ( allocated(this%ng ) ) deallocate(this%ng )
    if ( allocated(this%bnd) ) then
      call this%bnd%destroy()
      deallocate( this%bnd )
    end if
    this%n_blocks = -1
    this%n_dim    = -1
    this%n_bnd    = -1
  end subroutine destroy_block_info_type

  pure elemental function constructor( n_dim, n_blocks ) result( this )
    use combinatorics, only : n_hyper_cube_polytopes
    integer, intent(in) :: n_dim, n_blocks
    type(block_info_t)  :: this
    call this%destroy()
    this%n_dim    = int(n_dim,i8)
    this%n_blocks = int(n_blocks,i8)
    this%n_bnd    = int( n_hyper_cube_polytopes( n_dim, n_dim-1), i8 )
    allocate( this%id(n_blocks)  )
    allocate( this%n_off(n_blocks) )
    allocate( this%c_off(n_blocks) )
    allocate( this%sz(n_dim,n_blocks) )
    allocate( this%ng(n_dim,n_blocks) )
    this%id    = 0
    this%n_off = 0
    this%c_off = 0
    this%sz    = 0
    this%ng    = 0
    allocate( this%bnd( this%n_bnd, this%n_blocks ) )
    this%bnd = basic_bnd_info_t( n_dim, .false. )
  end function constructor

  pure function get_global_node_idx_from_local(this,block_id,loc_idx) result(glo_idx)
    use index_conversion, only : local2global
    class(block_info_t),       intent(in) :: this
    integer(i8),               intent(in) :: block_id
    integer(i8), dimension(:), intent(in) :: loc_idx
    integer(i8)                           :: glo_idx
    integer(i8) :: b
    integer(i8), dimension(this%n_dim) :: lo, hi
    b = findloc(this%id,block_id,dim=1)
    lo = 1 - this%ng(:,b)
    hi = this%sz(:,b) + this%ng(:,b)
    glo_idx = local2global( loc_idx(1:this%n_dim), lo, hi ) + this%n_off(b)
  end function get_global_node_idx_from_local

  pure function get_global_cell_idx_from_local(this,block_id,loc_idx) result(glo_idx)
    use index_conversion, only : local2global
    class(block_info_t),       intent(in) :: this
    integer(i8),               intent(in) :: block_id
    integer(i8), dimension(:), intent(in) :: loc_idx
    integer(i8)                           :: glo_idx
    integer(i8) :: b
    integer(i8), dimension(this%n_dim) :: lo, hi
    b = findloc(this%id,block_id,dim=1)
    lo = 1 - this%ng(:,b)
    hi = ( this%sz(:,b) - 1 ) + this%ng(:,b)
    glo_idx = local2global( loc_idx(1:this%n_dim), lo, hi ) + this%c_off(b)
  end function get_global_cell_idx_from_local

  pure function get_local_node_idx_from_global(this,block_id,glo_idx) result(loc_idx)
    use index_conversion, only : global2local
    class(block_info_t),    intent(in) :: this
    integer(i8),            intent(in) :: block_id
    integer(i8),            intent(in) :: glo_idx
    integer(i8), dimension(this%n_dim) :: loc_idx
    integer(i8) :: b
    integer(i8), dimension(this%n_dim) :: lo, hi
    b = findloc(this%id,block_id,dim=1)
    lo = 1 - this%ng(:,b)
    hi = this%sz(:,b) + this%ng(:,b)
    loc_idx = global2local(glo_idx - this%n_off(b), lo, hi )
  end function get_local_node_idx_from_global

  pure function get_local_cell_idx_from_global(this,block_id,glo_idx) result(loc_idx)
    use index_conversion, only : global2local
    class(block_info_t),    intent(in) :: this
    integer(i8),            intent(in) :: block_id
    integer(i8),            intent(in) :: glo_idx
    integer(i8), dimension(this%n_dim) :: loc_idx
    integer(i8) :: b
    integer(i8), dimension(this%n_dim) :: lo, hi
    b = findloc(this%id,block_id,dim=1)
    lo = 1 - this%ng(:,b)
    hi = ( this%sz(:,b) - 1 ) + this%ng(:,b)
    loc_idx = global2local(glo_idx - this%c_off(b), lo, hi )
  end function get_local_cell_idx_from_global

  pure subroutine setup_block_info_type( this, block_ids, block_sizes, block_n_ghosts )
    use index_conversion, only : local2global
    class(block_info_t),     intent(inout) :: this
    integer, dimension(:),   intent(in)    :: block_ids
    integer, dimension(:,:), intent(in)    :: block_sizes, block_n_ghosts
    integer(i8) :: i, j

    do i = 1,this%n_blocks
      this%id(i)   = int( block_ids(i),   i8 )
      this%sz(:,i) = int( block_sizes(1:this%n_dim,i), i8 )
      this%ng(:,i) = int( block_n_ghosts(1:this%n_dim,i), i8 )      
    end do

    this%n_off(1) = 0
    this%c_off(1) = 0
    do i = 2,this%n_blocks
      this%n_off(i) = this%n_off(i-1) + product( this%sz(:,i-1) + 2*this%ng(:,i-1) )
      this%c_off(i) = this%c_off(i-1) + product( this%sz(:,i-1) - 1 + 2*this%ng(:,i-1) )
    end do

    ! do i = 1,this%n_blocks
    !   do j = 1,this%n_bnd 
    !     this%bnd(j,i)%block_id = this%id(i)
    !     this%bnd(j,i)%loc_idx_lo = 1 - this%ng(:,b)
    !     this%bnd(j,i)%loc_idx_hi = this%sz(:,b) + this%ng(:,b)
    !     this%bnd(j,i)%glo_idx_lo = this%off(i) + 1
    !     this%bnd(j,i)%glo_idx_hi = this%off(i) + product(this%sz(:,i) + 2*this%ng(:,i))
        
  end subroutine setup_block_info_type

end module block_info_type

program main
  use set_precision, only : dp
  use set_constants, only : zero, one
  use block_info_type, only : block_info_t
  use combinatorics, only : unique_rand_ints_1D, rand_int_in_range
  implicit none

  type(block_info_t) :: info
  integer :: i, j, n_dim, n_blocks
  integer, parameter, dimension(2) :: sz_range = [5,201]
  integer, dimension(:),   allocatable :: block_ids
  integer, dimension(:,:), allocatable :: block_sizes, block_n_ghosts
  n_dim    = 4
  n_blocks = 3

  allocate( block_ids(n_blocks) )
  allocate( block_sizes(n_dim,n_blocks ) )
  allocate( block_n_ghosts(n_dim,n_blocks ) )
  block_n_ghosts = 2

  block_ids = unique_rand_ints_1D(1,n_blocks,n_blocks)
  do i = 1,n_blocks

    do j = 1,n_dim
      block_sizes(j,i) = rand_int_in_range(sz_range(1),sz_range(2))
    end do
  end do

  do j = 0,n_dim
    do i = 1,n_blocks
      info = block_info_t(j,i)
      call info%setup(block_ids,block_sizes,block_n_ghosts)
      write(*,'("(",I0,",",I0,"): n_bnd=",I0)') j, i, info%n_bnd
      call info%destroy()
    end do
  end do

  deallocate( block_ids, block_sizes, block_n_ghosts )

end program main