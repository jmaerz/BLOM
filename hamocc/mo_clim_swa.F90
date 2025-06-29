! Copyright (C) 2021  J. Tjiputra
!
! This file is part of BLOM/iHAMOCC.
!
! BLOM is free software: you can redistribute it and/or modify it under the
! terms of the GNU Lesser General Public License as published by the Free
! Software Foundation, either version 3 of the License, or (at your option)
! any later version.
!
! BLOM is distributed in the hope that it will be useful, but WITHOUT ANY
! WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
! FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for
! more details.
!
! You should have received a copy of the GNU Lesser General Public License
! along with BLOM. If not, see https://www.gnu.org/licenses/.


module mo_clim_swa

  !*************************************************************************************************
  ! Variables and routines for climatology short-wave fields
  ! -Declaration, memory allocation, and routines related to swa_clim fields
  !
  !  J.Tjiputra,        *NORCE Climate, Bergen*    2021-04-15
  !*************************************************************************************************

  use mo_kind, only: bgc_fnmlen,rp

  implicit none
  private

  ! Routines
  public :: ini_swa_clim

  ! Module variables

  ! File name (incl. full path) for input data, set through namelist in hamocc_init.F
  character(len=bgc_fnmlen), public :: swaclimfile=''

  ! Array to store swa flux after reading from file
  real(rp), allocatable, public :: swa_clim(:,:,:)

contains

  subroutine ini_swa_clim(kpie,kpje,omask)

    !***********************************************************************************************
    ! Initialise the climatology SWA field module, read in the swa (short-wave radiation) data set.
    !
    !  J.Tjiputra             *NORCE Climate, Bergen*       2021-04-15
    !***********************************************************************************************

    use netcdf,          only: nf90_noerr,nf90_nowrite,nf90_close,nf90_open
    use mod_xc,          only: mnproc,xchalt
    use mo_control_bgc,  only: io_stdo_bgc
    use mo_netcdf_bgcrw, only: read_netcdf_var

    ! Arguments
    integer, intent(in) :: kpie              !  1st dimension of model grid.
    integer, intent(in) :: kpje              !  2nd dimension of model grid.
    real(rp),intent(in) :: omask(kpie,kpje)  !  land/ocean mask (1=ocean)

    ! Local variables
    integer :: i,j
    integer :: ncid,ncstat,ncvarid,errstat

    ! allocate field to hold swa fields
    if (mnproc.eq.1) then
      write(io_stdo_bgc,*)' '
      write(io_stdo_bgc,*)'***************************************************'
      write(io_stdo_bgc,*)'iHAMOCC: Initialization of module mo_clim_swa:'
      write(io_stdo_bgc,*)' '
    endif

    if (mnproc.eq.1) then
      write(io_stdo_bgc,*)'Memory allocation for variable swa_clim ...'
      write(io_stdo_bgc,*)'First dimension    : ',kpie
      write(io_stdo_bgc,*)'Second dimension   : ',kpje
    endif
    allocate (swa_clim(kpie,kpje,1),stat=errstat)
    if(errstat.ne.0) stop 'not enough memory swa_clim'
    swa_clim(:,:,1) = 0.0_rp

    ! Open netCDF data file
    if (mnproc==1) then
      ncstat = NF90_OPEN(trim(swaclimfile),NF90_NOWRITE, ncid)
      if (ncstat /= NF90_NOERR ) then
        call xchalt('(ini_swa_clim: Problem with netCDF1)')
        stop        '(ini_swa_clim: Problem with netCDF1)'
      end if
    end if

    ! Read  data
    call read_netcdf_var(ncid,'swa',swa_clim(1,1,1),1,1,0)

    ! Close file
    if (mnproc==1) then
      ncstat = NF90_CLOSE(ncid)
      if ( ncstat  /=  NF90_NOERR ) then
        call xchalt('(ini_swa_clim: Problem with netCDF2)')
        stop        '(ini_swa_clim: Problem with netCDF2)'
      end if
    end if

    if (mnproc.eq.1) then
      write(io_stdo_bgc,*) ''
      write(io_stdo_bgc,*) 'ini_swa_clim: Using climatology swa file '//trim(swaclimfile)
    endif

    ! set flux to zero over land
    do j=1,kpje
      do i=1,kpie

        if(omask(i,j).lt.0.5_rp) swa_clim(i,j,1) = 0.0_rp

      enddo
    enddo

  end subroutine ini_swa_clim

end module mo_clim_swa
