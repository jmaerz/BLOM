! ------------------------------------------------------------------------------
! Copyright (C) 2004-2025 Mats Bentsen, Mehmet Ilicak, Aleksi Nummelin
!
! This file is part of BLOM.
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
! along with BLOM. If not, see <https://www.gnu.org/licenses/>.
! ------------------------------------------------------------------------------

module mod_sfcstr

  use mod_config,       only: expcnf
  use mod_xc,           only: lp, mnproc, xcstop
  use mod_sfcstr_cesm,  only: sfcstr_cesm
  use mod_sfcstr_ben02, only: sfcstr_ben02

  implicit none
  private

  public :: sfcstr

contains

  subroutine sfcstr(m, n, mm, nn, k1m, k1n)
  ! ---------------------------------------------------------------------------
  ! Get surface stress.
  ! ---------------------------------------------------------------------------

    ! Arguments
    integer, intent(in) :: m, n, mm, nn, k1m, k1n

    select case (trim(expcnf))
    case ('cesm')
      call sfcstr_cesm()
    case ('ben02clim', 'ben02syn', 'single_column')
      call sfcstr_ben02()
    case ('noforcing')
    case ('fuk95')
    case ('channel')
    case ('isomip1')
      ! call sfcstr_isomip1(m, n, mm, nn, k1m, k1n)
    case ('isomip2')
      ! call sfcstr_isomip2(m, n, mm, nn, k1m, k1n)
    case default
      if (mnproc == 1) then
        write (lp,'(3a)') ' sfcstr: expcnf = ', trim(expcnf), &
             ' is unsupported!'
      endif
      call xcstop('(sfcstr)')
      stop '(sfcstr)'
    end select

  end subroutine sfcstr

end module mod_sfcstr
