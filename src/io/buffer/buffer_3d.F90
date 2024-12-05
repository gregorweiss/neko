! Copyright (c) 2024, Gregor Weiss (HLRS)
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
!> Generic buffer that is extended with buffers of varying rank
module buffer_3d
  use num_types
  use vector
#ifdef HAVE_ADIOS2_FORTRAN
  use adios2
#endif
  use buffer
  implicit none

  type, extends(buffer_t) :: buffer_3d_t
     integer :: nelx, nely, nelz, lx, ly, lz
     integer(kind=8), dimension(3) :: shape_dims, start_dims, count_dims
     real(kind=dp), private, allocatable :: data_dp(:,:,:)
     real(kind=sp), private, allocatable :: data_sp(:,:,:)
   contains
     procedure :: init => buffer_3d_init
     procedure :: fill => buffer_3d_fill
#ifdef HAVE_ADIOS2_FORTRAN
     procedure :: define => buffer_3d_define
     procedure :: inquire => buffer_3d_inquire
     procedure :: write => buffer_3d_write
     procedure :: read => buffer_3d_read
#endif
     procedure :: copy => buffer_3d_copy
  end type buffer_3d_t

contains

  subroutine buffer_3d_init(this, precision, gdim, glb_nelv, offset_el, nelv, lx, ly, lz, lts, lpar)
    class(buffer_3d_t), intent(inout) :: this
    logical, intent(in) :: precision
    integer, intent(in) :: gdim, glb_nelv, offset_el, nelv, lx, ly, lz, lts, lpar
    integer :: nelx, nely, nelz

    nelx = glb_nelv !> @todo unhack
    nely = offset_el
    nelz = nelv
    this%nelx = nelx !> @todo unhack
    this%nely = nely
    this%nelz = nelz
    this%lx = lx
    this%ly = ly
    this%lz = lz

    call buffer_set_precision(this, precision)

    if (this%dp_precision) then
       if (allocated(this%data_dp)) then
          deallocate(this%data_dp)
       end if
       allocate(this%data_dp(nelx*lx, nely*ly, nelz*lz))
    else
       if (allocated(this%data_sp)) then
          deallocate(this%data_sp)
       end if
       allocate(this%data_sp(nelx*lx, nely*ly, nelz*lz))
    end if

    this%shape_dims = [int(nelx*lx, i8), int(nely*ly, i8), int(nelz*lz, i8)]
    this%start_dims = [int(0, i8), int(0, i8), int(0, i8)]
    this%count_dims = [int(nelx*lx, i8), int(nely*ly, i8), int(nelz*lz, i8)]

  end subroutine buffer_3d_init

  subroutine buffer_3d_fill(this, x, n)
    class(buffer_3d_t), intent(inout) :: this
    integer, intent(inout) :: n
    real(kind=rp), intent(inout) :: x(n)
    integer :: i, j, k, l, m, nelv, lx, ly, lz, nelx, nely, nelz, index
    integer :: c1, c2, c3, c4, c5

    lx = this%lx
    ly = this%ly
    lz = this%lz
    nelx = this%nelx
    nely = this%nely
    nelz = this%nelz

    c1 = lx
    c2 = lx*ly
    c3 = lx*ly*lz
    c4 = lx*ly*lz*nelx
    c5 = lx*ly*lz*nelx*nely

    if (this%dp_precision) then
       do i = 1, nelz
          do j = 1, nely
             do k = 1, nelx
                do l = 1, lz
                   do m = 1, ly
                      do n = 1, lx !> @todo unhack name of 'n'
                   index = (n-1) + c1*(m-1) + c2*(l-1) + c3*(k-1) + c4*(j-1) + c5*(i-1) + 1
                   this%data_dp(n+lx*(k-1), m+ly*(j-1), l+lz*(i-1)) = real(x(index),dp)
                      end do
                   end do
                end do
             end do
          end do
       end do
    else
       do i = 1, nelz
          do j = 1, nely
             do k = 1, nelx
                do l = 1, lz
                   do m = 1, ly
                      do n = 1, lx !> @todo unhack name of 'n'
                   index = (n-1) + c1*(m-1) + c2*(l-1) + c3*(k-1) + c4*(j-1) + c5*(i-1) + 1
                   this%data_sp(n+lx*(k-1), m+ly*(j-1), l+lz*(i-1)) = real(x(index),sp)
                      end do
                   end do
                end do
             end do
          end do
       end do
    end if

  end subroutine buffer_3d_fill

#ifdef HAVE_ADIOS2_FORTRAN

  subroutine buffer_3d_define(this, variable, io, variable_name, ierr)
    class(buffer_3d_t), intent(inout) :: this
    type(adios2_variable), intent(inout) :: variable
    type(adios2_io), intent(inout) :: io
    character(len=*), intent(in) :: variable_name
    integer, intent(inout) :: ierr
    integer :: adios2_type

    if (this%dp_precision) then
       adios2_type = adios2_type_dp
    else
       adios2_type = adios2_type_real
    end if

    call adios2_inquire_variable(variable, io, trim(variable_name), ierr)
    if (.not.variable%valid) then
       !> @todo could the shape and slice be fixed?
       call adios2_define_variable(variable, io, variable_name, adios2_type, &
            size(this%shape_dims), this%shape_dims, this%start_dims, &
            this%count_dims, .false., ierr)
    else
       call adios2_set_selection(variable, size(this%start_dims), &
            this%start_dims, this%count_dims, ierr)
    end if

  end subroutine buffer_3d_define

  subroutine buffer_3d_inquire(this, variable, io, variable_name, ierr)
    class(buffer_3d_t), intent(inout) :: this
    type(adios2_variable), intent(inout) :: variable
    type(adios2_io), intent(inout) :: io
    character(len=*), intent(in) :: variable_name
    integer, intent(inout) :: ierr

    call adios2_inquire_variable(variable, io, trim(variable_name), ierr)
    if (variable%valid) then
       call adios2_set_selection(variable, size(this%start_dims), &
            this%start_dims, this%count_dims, ierr)
    end if

  end subroutine buffer_3d_inquire

  subroutine buffer_3d_write(this, engine, variable, ierr)
    class(buffer_3d_t), intent(inout) :: this
    type(adios2_engine), intent(in) :: engine
    type(adios2_variable), intent(in) :: variable
    integer, intent(inout) :: ierr

    if (this%dp_precision) then
       call adios2_put(engine, variable, this%data_dp, adios2_mode_sync, ierr)
    else
       call adios2_put(engine, variable, this%data_sp, adios2_mode_sync, ierr)
    end if

  end subroutine buffer_3d_write
  
  subroutine buffer_3d_read(this, engine, variable, ierr)
    class(buffer_3d_t), intent(inout) :: this
    type(adios2_engine), intent(in) :: engine
    type(adios2_variable), intent(in) :: variable
    integer, intent(inout) :: ierr

    if (this%dp_precision) then
       call adios2_get(engine, variable, this%data_dp, adios2_mode_sync, ierr)
    else
       call adios2_get(engine, variable, this%data_sp, adios2_mode_sync, ierr)
    end if

  end subroutine buffer_3d_read

#endif

  subroutine buffer_3d_copy(this, x)
    class(buffer_3d_t), intent(inout) :: this
    type(vector_t), intent(inout) :: x
    integer :: i, j, k, l, m, n, nelv, lx, ly, lz, nelx, nely, nelz, index
    integer :: c1, c2, c3, c4, c5

    lx = this%lx
    ly = this%ly
    lz = this%lz
    nelx = this%nelx
    nely = this%nely
    nelz = this%nelz

    c1 = lx
    c2 = lx*ly
    c3 = lx*ly*lz
    c4 = lx*ly*lz*nelx
    c5 = lx*ly*lz*nelx*nely

    if (this%dp_precision) then
       do i = 1, nelz
          do j = 1, nely
             do k = 1, nelx
                do l = 1, lz
                   do m = 1, ly
                      do n = 1, lx !> @todo unhack name of 'n'
                   index = (n-1) + c1*(m-1) + c2*(l-1) + c3*(k-1) + c4*(j-1) + c5*(i-1) + 1
                   x%x(index) = this%data_dp(n+lx*(k-1), m+ly*(j-1), l+lz*(i-1))
                      end do
                   end do
                end do
             end do
          end do
       end do
    else
       do i = 1, nelz
          do j = 1, nely
             do k = 1, nelx
                do l = 1, lz
                   do m = 1, ly
                      do n = 1, lx !> @todo unhack name of 'n'
                   index = (n-1) + c1*(m-1) + c2*(l-1) + c3*(k-1) + c4*(j-1) + c5*(i-1) + 1
                   x%x(index) = this%data_sp(n+lx*(k-1), m+ly*(j-1), l+lz*(i-1))
                      end do
                   end do
                end do
             end do
          end do
       end do
    end if

  end subroutine buffer_3d_copy

end module buffer_3d
