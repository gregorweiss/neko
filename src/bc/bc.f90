! Copyright (c) 2020-2024, The Neko Authors
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions
! are met:
!
!   * Redistributions of source code must retain the above copyright
!     notice, this list of conditions and the following disclaimer.
!
!   * Redistributions in binary form must reproduce the above
!     copyright notice, this list of conditions and the following
!     disclaimer in the documentation and/or other materials provided
!     with the distribution.
!
!   * Neither the name of the authors nor the names of its
!     contributors may be used to endorse or promote products derived
!     from this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
! FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
! COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
! BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
! ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.
!
!> Defines a boundary condition
module bc
  use neko_config
  use num_types
  use device
  use dofmap, only : dofmap_t
  use coefs, only : coef_t
  use space, only : space_t
  use mesh, only : mesh_t, NEKO_MSH_MAX_ZLBLS, NEKO_MSH_MAX_ZLBL_LEN
  use facet_zone, only : facet_zone_t
  use stack, only : stack_i4t2_t
  use tuple, only : tuple_i4_t
  use utils, only : neko_error, linear_index, split_string
  use, intrinsic :: iso_c_binding, only : c_ptr, C_NULL_PTR
  implicit none
  private

  !> Base type for a boundary condition
  type, public, abstract :: bc_t
     !> The linear index of each node in each boundary facet
     integer, allocatable :: msk(:)
     !> A list of facet ids (1 to 6), one for each element in msk
     integer, allocatable :: facet(:)
     !> Map of degrees of freedom
     type(dofmap_t), pointer :: dof
     !> SEM coefficients
     type(coef_t), pointer :: coef
     !> The mesh
     type(mesh_t), pointer :: msh
     !> The function space
     type(space_t), pointer :: Xh
     !> Index tuples (facet, element) marked as part of the boundary condition
     type(stack_i4t2_t) :: marked_facet
     !> Device pointer for msk
     type(c_ptr) :: msk_d = C_NULL_PTR
     !> Device pointer for facet
     type(c_ptr) :: facet_d = C_NULL_PTR
   contains
     !> Constructor
     procedure, pass(this) :: init_base => bc_init_base
     !> Destructor
     procedure, pass(this) :: free_base => bc_free_base
     !> Mark a facet on an element as part of the boundary condition
     procedure, pass(this) :: mark_facet => bc_mark_facet
     !> Mark all facets from a (facet, element) tuple list
     procedure, pass(this) :: mark_facets => bc_mark_facets
     !> Mark all facets from a list of zones, also marks type of bc in the mesh.
     procedure, pass(this) :: mark_zones_from_list => bc_mark_zones_from_list
     !> Mark all facets from a zone
     procedure, pass(this) :: mark_zone => bc_mark_zone
     !> Finalize the construction of the bc by populting the msk and facet
     !! arrays
     procedure, pass(this) :: finalize => bc_finalize
     !> Apply the boundary condition to a scalar field
     procedure(bc_apply_scalar), pass(this), deferred :: apply_scalar
     !> Apply the boundary condition to a vector field
     procedure(bc_apply_vector), pass(this), deferred :: apply_vector
     !> Device version of \ref apply_scalar
     procedure(bc_apply_scalar_dev), pass(this), deferred :: apply_scalar_dev
     !> Device version of \ref apply_vector
     procedure(bc_apply_vector_dev), pass(this), deferred :: apply_vector_dev
     !> Destructor
     procedure(bc_destructor), pass(this), deferred :: free
  end type bc_t

  !> Pointer to boundary condtiion
  type, private :: bcp_t
     class(bc_t), pointer :: bcp
  end type bcp_t

  !> A list of boundary conditions
  type, public :: bc_list_t
     type(bcp_t), allocatable :: bc(:)
     !> Number of items.
     integer :: n
     !> Capacity.
     integer :: size
   contains
     procedure, public, pass(this) :: get_size => bc_list_size
     procedure, public, pass(this) :: is_empty => bc_list_is_empty
  end type bc_list_t

  abstract interface
     !> Apply the boundary condition to a scalar field
     !! @param x The field for which to apply the boundary condition.
     !! @param n The size of x.
     !! @param t Current time.
     !! @param tstep Current time-step.
     subroutine bc_apply_scalar(this, x, n, t, tstep)
       import :: bc_t
       import :: rp
       class(bc_t), intent(inout) :: this
       integer, intent(in) :: n
       real(kind=rp), intent(inout), dimension(n) :: x
       real(kind=rp), intent(in), optional :: t
       integer, intent(in), optional :: tstep
     end subroutine bc_apply_scalar
  end interface

  abstract interface
     !> Apply the boundary condition to a vector field
     !! @param x The x comp of the field for which to apply the bc.
     !! @param y The y comp of the field for which to apply the bc.
     !! @param z The z comp of the field for which to apply the bc.
     !! @param n The size of x, y, and z.
     !! @param t Current time.
     !! @param tstep Current time-step.
     subroutine bc_apply_vector(this, x, y, z, n, t, tstep)
       import :: bc_t
       import :: rp
       class(bc_t), intent(inout) :: this
       integer, intent(in) :: n
       real(kind=rp), intent(inout), dimension(n) :: x
       real(kind=rp), intent(inout), dimension(n) :: y
       real(kind=rp), intent(inout), dimension(n) :: z
       real(kind=rp), intent(in), optional :: t
       integer, intent(in), optional :: tstep
     end subroutine bc_apply_vector
  end interface

  abstract interface
     !> Destructor
     subroutine bc_destructor(this)
       import :: bc_t
       class(bc_t), intent(inout), target :: this
     end subroutine bc_destructor
  end interface

  abstract interface
     !> Apply the boundary condition to a scalar field on the device
     !! @param x_d Device pointer to the field.
     subroutine bc_apply_scalar_dev(this, x_d, t, tstep)
       import :: c_ptr
       import :: bc_t
       import :: rp
       class(bc_t), intent(inout), target :: this
       type(c_ptr) :: x_d
       real(kind=rp), intent(in), optional :: t
       integer, intent(in), optional :: tstep
     end subroutine bc_apply_scalar_dev
  end interface

  abstract interface
     !> Apply the boundary condition to a vector field on the device
     !! @param x_d Device pointer to the values to be applied for the x comp.
     !! @param y_d Device pointer to the values to be applied for the y comp.
     !! @param z_d Device pointer to the values to be applied for the z comp.
     subroutine bc_apply_vector_dev(this, x_d, y_d, z_d, t, tstep)
       import :: c_ptr
       import :: bc_t
       import :: rp
       class(bc_t), intent(inout), target :: this
       type(c_ptr) :: x_d
       type(c_ptr) :: y_d
       type(c_ptr) :: z_d
       real(kind=rp), intent(in), optional :: t
       integer, intent(in), optional :: tstep
     end subroutine bc_apply_vector_dev
  end interface

  interface bc_list_apply
     module procedure bc_list_apply_scalar, bc_list_apply_vector
  end interface bc_list_apply

  public :: bc_list_init, bc_list_free, bc_list_add, &
  bc_list_apply_scalar, bc_list_apply_vector, bc_list_apply

contains

  !> Constructor
  !! @param dof Map of degrees of freedom.
  subroutine bc_init_base(this, coef)
    class(bc_t), intent(inout) :: this
    type(coef_t), target, intent(in) :: coef

    call this%free_base

    this%dof => coef%dof
    this%coef => coef
    this%Xh => this%dof%Xh
    this%msh => this%dof%msh

    call this%marked_facet%init()

  end subroutine bc_init_base

  !> Destructor for the base type, `bc_t`.
  subroutine bc_free_base(this)
    class(bc_t), intent(inout) :: this

    call this%marked_facet%free()

    nullify(this%Xh)
    nullify(this%msh)
    nullify(this%dof)
    nullify(this%coef)

    if (allocated(this%msk)) then
       deallocate(this%msk)
    end if

    if (allocated(this%facet)) then
       deallocate(this%facet)
    end if

    if (c_associated(this%msk_d)) then
       call device_free(this%msk_d)
       this%msk_d = C_NULL_PTR
    end if

    if (c_associated(this%facet_d)) then
       call device_free(this%facet_d)
       this%facet_d = C_NULL_PTR
    end if

  end subroutine bc_free_base

  !> Mark @a facet on element @a el as part of the boundary condition
  !! @param facet The index of the facet.
  !! @param el The index of the element.
  subroutine bc_mark_facet(this, facet, el)
    class(bc_t), intent(inout) :: this
    integer, intent(in) :: facet
    integer, intent(in) :: el
    type(tuple_i4_t) :: t

    t%x = (/facet, el/)
    call this%marked_facet%push(t)

  end subroutine bc_mark_facet

  !> Mark all facets from a (facet, el) tuple list
  !! @param facet_list The list of tuples.
  subroutine bc_mark_facets(this, facet_list)
    class(bc_t), intent(inout) :: this
    type(stack_i4t2_t), intent(inout) :: facet_list
    type(tuple_i4_t), pointer :: fp(:)
    integer :: i

    fp => facet_list%array()
    do i = 1, facet_list%size()
       call this%marked_facet%push(fp(i))
    end do

  end subroutine bc_mark_facets

  !> Mark all facets from a zone
  !! @param bc_zone Boundary zone to be marked.
  subroutine bc_mark_zone(this, bc_zone)
    class(bc_t), intent(inout) :: this
    class(facet_zone_t), intent(in) :: bc_zone
    integer :: i
    do i = 1, bc_zone%size
       call this%marked_facet%push(bc_zone%facet_el(i))
    end do
  end subroutine bc_mark_zone

  !> Mark all facets from a list of zones, also marks type of bc in the mesh.
  !! The facet_type in mesh is because of the fdm from Nek5000...
  !! That is a hack that should be removed at some point...
  !! @param bc_zone Array of boundary zones.
  !! @param bc_key Boundary condition label, e.g. 'w' for wall.
  !! @param bc_label List of boundary condition labels.
  subroutine bc_mark_zones_from_list(this, bc_zones, bc_key, bc_labels)
    class(bc_t), intent(inout) :: this
    class(facet_zone_t), intent(in) :: bc_zones(:)
    character(len=*) :: bc_key
    character(len=100), allocatable :: split_key(:)
    character(len=NEKO_MSH_MAX_ZLBL_LEN) :: bc_labels(:)
    integer :: i, j, k, l, msh_bc_type

    msh_bc_type = 0
    if(trim(bc_key) .eq. 'o' .or. trim(bc_key) .eq. 'on' &
       .or. trim(bc_key) .eq. 'o+dong' .or. trim(bc_key) .eq. 'on+dong') then
       msh_bc_type = 1
    else if(trim(bc_key) .eq. 'd_pres') then
       msh_bc_type = 1
    else if(trim(bc_key) .eq. 'w') then
       msh_bc_type = 2
    else if(trim(bc_key) .eq. 'v') then
       msh_bc_type = 2
    else if(trim(bc_key) .eq. 'd_vel_u') then
       msh_bc_type = 2
    else if(trim(bc_key) .eq. 'd_vel_v') then
       msh_bc_type = 2
    else if(trim(bc_key) .eq. 'd_vel_w') then
       msh_bc_type = 2
    else if(trim(bc_key) .eq. 'sym') then
       msh_bc_type = 2
    else if(trim(bc_key) .eq. 'sh') then
       msh_bc_type = 2
    else if(trim(bc_key) .eq. 'wm') then
       msh_bc_type = 2
    end if

    do i = 1, size(bc_labels)
       !Check if several bcs are defined for this zone
       !bcs are seperated by /, but we could use something else
       if (index(trim(bc_labels(i)), '/') .eq. 0) then
          if (trim(bc_key) .eq. trim(bc_labels(i))) then
             call bc_mark_zone(this, bc_zones(i))
             ! Loop across all faces in the mesh
             do j = 1,this%msh%nelv
                do k = 1, 2 * this%msh%gdim
                   if (this%msh%facet_type(k,j) .eq. -i) then
                      this%msh%facet_type(k,j) = msh_bc_type
                   end if
                end do
             end do
          end if
       else
          split_key = split_string(trim(bc_labels(i)),'/')
          do l = 1, size(split_key)
             if (trim(split_key(l)) .eq. trim(bc_key)) then
                call bc_mark_zone(this, bc_zones(i))
                ! Loop across all faces in the mesh
                do j = 1,this%msh%nelv
                   do k = 1, 2 * this%msh%gdim
                      if (this%msh%facet_type(k,j) .eq. -i) then
                         this%msh%facet_type(k,j) = msh_bc_type
                      end if
                   end do
                end do
             end if
          end do
       end if
    end do
  end subroutine bc_mark_zones_from_list


  !> Finalize the construction of the bc by populting the `msk` and `facet`
  !! arrays.
  !! @details This will linearize the marked facet's indicies in the msk array.
  subroutine bc_finalize(this)
    class(bc_t), target, intent(inout) :: this
    type(tuple_i4_t), pointer :: bfp(:)
    type(tuple_i4_t) :: bc_facet
    integer :: facet_size, facet, el
    integer :: i, j, k, l, msk_c
    integer :: lx, ly, lz, n

    lx = this%Xh%lx
    ly = this%Xh%ly
    lz = this%Xh%lz

    !>@todo add 2D case

    ! Note we assume that lx = ly = lz
    facet_size = lx**2
    allocate(this%msk(0:facet_size * this%marked_facet%size()))
    allocate(this%facet(0:facet_size * this%marked_facet%size()))

    msk_c = 0
    bfp => this%marked_facet%array()

    ! Loop through each (facet, element) id tuple
    ! Then loop over all the nodes of the face and compute their linear index
    ! This index goes into this%msk, whereas the corresponding face id goes into
    ! this%facet
    do i = 1, this%marked_facet%size()
       bc_facet = bfp(i)
       facet = bc_facet%x(1)
       el = bc_facet%x(2)
       select case (facet)
       case (1)
          do l = 1, lz
             do k = 1, ly
                msk_c = msk_c + 1
                this%msk(msk_c) = linear_index(1,k,l,el,lx,ly,lz)
                this%facet(msk_c) = 1
             end do
          end do
       case (2)
          do l = 1, lz
             do k = 1, ly
                msk_c = msk_c + 1
                this%msk(msk_c) = linear_index(lx,k,l,el,lx,ly,lz)
                this%facet(msk_c) = 2
             end do
          end do
       case(3)
          do l = 1, lz
             do j = 1, lx
                msk_c = msk_c + 1
                this%msk(msk_c) = linear_index(j,1,l,el,lx,ly,lz)
                this%facet(msk_c) = 3
             end do
          end do
       case(4)
          do l = 1, lz
             do j = 1, lx
                msk_c = msk_c + 1
                this%msk(msk_c) = linear_index(j,ly,l,el,lx,ly,lz)
                this%facet(msk_c) = 4
             end do
          end do
       case(5)
          do k = 1, ly
             do j = 1, lx
                msk_c = msk_c + 1
                this%msk(msk_c) = linear_index(j,k,1,el,lx,ly,lz)
                this%facet(msk_c) = 5
             end do
          end do
       case(6)
          do k = 1, ly
             do j = 1, lx
                msk_c = msk_c + 1
                this%msk(msk_c) = linear_index(j,k,lz,el,lx,ly,lz)
                this%facet(msk_c) = 6
             end do
          end do
       end select
    end do

    this%msk(0) = msk_c
    this%facet(0) = msk_c

    if (NEKO_BCKND_DEVICE .eq. 1) then
       n = facet_size * this%marked_facet%size() + 1
       call device_map(this%msk, this%msk_d, n)
       call device_map(this%facet, this%facet_d, n)

       call device_memcpy(this%msk, this%msk_d, n, &
                          HOST_TO_DEVICE, sync=.false.)
       call device_memcpy(this%facet, this%facet_d, n, &
                          HOST_TO_DEVICE, sync=.false.)
    end if

  end subroutine bc_finalize

  !> Constructor for a list of boundary conditions
  !! @param size The size of the list to allocate.
  subroutine bc_list_init(bclst, size)
    type(bc_list_t), intent(inout), target :: bclst
    integer, optional :: size
    integer :: n, i

    call bc_list_free(bclst)

    if (present(size)) then
       n = size
    else
       n = 1
    end if

    allocate(bclst%bc(n))

    do i = 1, n
       bclst%bc(i)%bcp => null()
    end do

    bclst%n = 0
    bclst%size = n

  end subroutine bc_list_init

  !> Destructor for a list of boundary conditions
  !! @note This will only nullify all pointers, not deallocate any
  !! conditions pointed to by the list
  subroutine bc_list_free(bclst)
    type(bc_list_t), intent(inout) :: bclst

    if (allocated(bclst%bc)) then
       deallocate(bclst%bc)
    end if

    bclst%n = 0
    bclst%size = 0

  end subroutine bc_list_free

  !> Return number of bcs in the list
  function bc_list_size(this) result(n)
    class(bc_list_t), intent(inout) :: this
    integer :: n

    n = this%n

  end function bc_list_size
  
  !> Check if bclist is empty (even if someone has tampered with it)
  function bc_list_is_empty(this) result(is_empty)
    class(bc_list_t), intent(inout) :: this
    logical :: is_empty
    integer :: i
    
    is_empty = .true. 
    do i = 1, this%get_size()

       if (.not. allocated(this%bc(i)%bcp%msk)) then
          call neko_error("bc not finalized, error in bc_list_is_empty")
       end if

       if (this%bc(i)%bcp%msk(0) > 0) is_empty = .false.
    
    end do

  end function bc_list_is_empty



  !> Add a condition to a list of boundary conditions
  !! @param bc The boundary condition to add.
  subroutine bc_list_add(bclst, bc)
    type(bc_list_t), intent(inout) :: bclst
    class(bc_t), intent(inout), target :: bc
    type(bcp_t), allocatable :: tmp(:)

    !> Do not add if bc is empty
    if(bc%marked_facet%size() .eq. 0) return

    if (bclst%n .ge. bclst%size) then
       bclst%size = bclst%size * 2
       allocate(tmp(bclst%size))
       tmp(1:bclst%n) = bclst%bc
       call move_alloc(tmp, bclst%bc)
    end if

    bclst%n = bclst%n + 1
    bclst%bc(bclst%n)%bcp => bc

  end subroutine bc_list_add

  !> Apply a list of boundary conditions to a scalar field
  !! @param x The field to apply the boundary conditions to.
  !! @param n The size of x.
  !! @param t Current time.
  !! @param tstep Current time-step.
  subroutine bc_list_apply_scalar(bclst, x, n, t, tstep)
    type(bc_list_t), intent(in) :: bclst
    integer, intent(in) :: n
    real(kind=rp), intent(inout), dimension(n) :: x
    real(kind=rp), intent(in), optional :: t
    integer, intent(in), optional :: tstep
    type(c_ptr) :: x_d
    integer :: i

    if (NEKO_BCKND_DEVICE .eq. 1) then
       x_d = device_get_ptr(x)
       if (present(t) .and. present(tstep)) then
          do i = 1, bclst%n
             call bclst%bc(i)%bcp%apply_scalar_dev(x_d, t=t, tstep=tstep)
          end do
       else if (present(t)) then
          do i = 1, bclst%n
             call bclst%bc(i)%bcp%apply_scalar_dev(x_d, t=t)
          end do
       else if (present(tstep)) then
          do i = 1, bclst%n
             call bclst%bc(i)%bcp%apply_scalar_dev(x_d, tstep=tstep)
          end do
       else
          do i = 1, bclst%n
             call bclst%bc(i)%bcp%apply_scalar_dev(x_d)
          end do
       end if
    else
       if (present(t) .and. present(tstep)) then
          do i = 1, bclst%n
             call bclst%bc(i)%bcp%apply_scalar(x, n, t, tstep)
          end do
       else if (present(t)) then
          do i = 1, bclst%n
             call bclst%bc(i)%bcp%apply_scalar(x, n, t=t)
          end do
       else if (present(tstep)) then
          do i = 1, bclst%n
             call bclst%bc(i)%bcp%apply_scalar(x, n, tstep=tstep)
          end do
       else
          do i = 1, bclst%n
             call bclst%bc(i)%bcp%apply_scalar(x, n)
          end do
       end if
    end if

  end subroutine bc_list_apply_scalar

  !> Apply a list of boundary conditions to a vector field.
  !! @param x The x comp of the field for which to apply the bcs.
  !! @param y The y comp of the field for which to apply the bcs.
  !! @param z The z comp of the field for which to apply the bcs.
  !! @param n The size of x, y, z.
  !! @param t Current time.
  !! @param tstep Current time-step.
  subroutine bc_list_apply_vector(bclst, x, y, z, n, t, tstep)
    type(bc_list_t), intent(in) :: bclst
    integer, intent(in) :: n
    real(kind=rp), intent(inout),  dimension(n) :: x
    real(kind=rp), intent(inout),  dimension(n) :: y
    real(kind=rp), intent(inout),  dimension(n) :: z
    real(kind=rp), intent(in), optional :: t
    integer, intent(in), optional :: tstep
    type(c_ptr) :: x_d
    type(c_ptr) :: y_d
    type(c_ptr) :: z_d
    integer :: i

    if (NEKO_BCKND_DEVICE .eq. 1) then
       x_d = device_get_ptr(x)
       y_d = device_get_ptr(y)
       z_d = device_get_ptr(z)
       if (present(t) .and. present(tstep)) then
          do i = 1, bclst%n
             call bclst%bc(i)%bcp%apply_vector_dev(x_d, y_d, z_d, t, tstep)
          end do
       else if (present(t)) then
          do i = 1, bclst%n
             call bclst%bc(i)%bcp%apply_vector_dev(x_d, y_d, z_d, t=t)
          end do
       else if (present(tstep)) then
          do i = 1, bclst%n
             call bclst%bc(i)%bcp%apply_vector_dev(x_d, y_d, z_d, tstep=tstep)
          end do
       else
          do i = 1, bclst%n
             call bclst%bc(i)%bcp%apply_vector_dev(x_d, y_d, z_d)
          end do
       end if
    else
       if (present(t) .and. present(tstep)) then
          do i = 1, bclst%n
             call bclst%bc(i)%bcp%apply_vector(x, y, z, n, t, tstep)
          end do
       else if (present(t)) then
          do i = 1, bclst%n
             call bclst%bc(i)%bcp%apply_vector(x, y, z, n, t=t)
          end do
       else if (present(tstep)) then
          do i = 1, bclst%n
             call bclst%bc(i)%bcp%apply_vector(x, y, z, n, tstep=tstep)
          end do
       else
          do i = 1, bclst%n
             call bclst%bc(i)%bcp%apply_vector(x, y, z, n)
          end do
       end if
    end if

  end subroutine bc_list_apply_vector


end module bc
