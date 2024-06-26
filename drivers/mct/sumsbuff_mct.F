! ------------------------------------------------------------------------------
! Copyright (C) 2008-2021 Mats Bentsen, Jerry Tjiputra, Jörg Schwinger
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

      subroutine sumsbuff_mct(nsend, sbuff, tlast_coupled)

      ! Uses modules
      
      use mod_types, only: r8
      use mod_time, only: nstep, baclin, delt1, dlt
      use mod_xc
      use mod_grid, only: scuy, scvx, scuxi, scvyi
      use mod_state, only: u, v, temp, saln, pbu, pbv, ubflxs, vbflxs,
     .                     sealv
      use mod_forcing, only: flxco2, flxdms, flxbrf,flxn2o,flxnh3
      use mod_cesm, only: frzpot
      use blom_cpl_indices

      implicit none

      ! Input/output arguments

      integer, intent(in) :: nsend
      real(r8), dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,nsend),
     .   intent(inout) :: sbuff
      real(r8), intent(inout) :: tlast_coupled

      ! Local variables

      integer i, j, l, k, m, n, mm, nn, k1m, k1n

      ! External functions

      !-----------------------------------------------------------------
      ! Set send buffer to zero if this is the first call after a
      ! coupling interval
      !-----------------------------------------------------------------

      if (tlast_coupled == 0._r8) then
         do k = 1, nsend
            do j = 1-nbdy, jdm+nbdy
               do i = 1-nbdy, idm+nbdy
                  sbuff(i,j,k) = 0._r8
               enddo
            enddo
         enddo
      endif

      !-----------------------------------------------------------------
      ! Accumulate fields in send buffer
      !-----------------------------------------------------------------

      m=mod(nstep+1,2)+1
      n=mod(nstep  ,2)+1
      mm=(m-1)*kk
      nn=(n-1)*kk
      k1m=1+mm
      k1n=1+nn

      call xctilr(sealv, 1,1, 1,1, halo_ps)
      
      do j = 1, jj
         do l = 1, isu(j)
         do i = max(1,ifu(j,l)), min(ii,ilu(j,l))
            sbuff(i,j,index_o2x_So_u) =
     .         sbuff(i,j,index_o2x_So_u)
     .       + ( u(i,j,k1n) 
     .         + (ubflxs(i,j,m) + ubflxs(i,j,n))*dlt
     .           /(pbu(i,j,n)*scuy(i,j)*delt1))*baclin
            sbuff(i,j,index_o2x_So_dhdx) =
     .         sbuff(i,j,index_o2x_So_dhdx)
     .       + (sealv(i,j)-sealv(i-1,j))*scuxi(i,j)*baclin
         enddo
         enddo
      enddo

      do j = 1, jj
         do l = 1, isv(j)
         do i = max(1,ifv(j,l)), min(ii,ilv(j,l))
            sbuff(i,j,index_o2x_So_v) =
     .         sbuff(i,j,index_o2x_So_v)
     .       + ( v(i,j,k1n) 
     .         + (vbflxs(i,j,m) + vbflxs(i,j,n))*dlt
     .           /(pbv(i,j,n)*scvx(i,j)*delt1))*baclin
            sbuff(i,j,index_o2x_So_dhdy) =
     .         sbuff(i,j,index_o2x_So_dhdy)
     .       + (sealv(i,j)-sealv(i,j-1))*scvyi(i,j)*baclin
         enddo
         enddo
      enddo

      do j = 1, jj
         do l = 1, isp(j)
         do i = max(1,ifp(j,l)), min(ii,ilp(j,l))
            sbuff(i,j,index_o2x_So_t) =
     .         sbuff(i,j,index_o2x_So_t) + temp(i,j,k1n)*baclin
            sbuff(i,j,index_o2x_So_s) =
     .         sbuff(i,j,index_o2x_So_s) + saln(i,j,k1n)*baclin
            sbuff(i,j,index_o2x_Fioo_q) =
     .         sbuff(i,j,index_o2x_Fioo_q) + frzpot(i,j)
         enddo
         enddo
      enddo

      if (index_o2x_Faoo_fco2_ocn > 0) then
         do j = 1, jj
            do l = 1, isp(j)
            do i = max(1,ifp(j,l)), min(ii,ilp(j,l))
               sbuff(i,j,index_o2x_Faoo_fco2_ocn) =
     .            sbuff(i,j,index_o2x_Faoo_fco2_ocn)
     .          + flxco2(i,j)*baclin
            enddo
            enddo
         enddo
      endif

      if (index_o2x_Faoo_fdms_ocn > 0) then
         do j = 1, jj
            do l = 1, isp(j)
            do i = max(1,ifp(j,l)), min(ii,ilp(j,l))
               sbuff(i,j,index_o2x_Faoo_fdms_ocn) =
     .            sbuff(i,j,index_o2x_Faoo_fdms_ocn)
     .          + flxdms(i,j)*baclin
            enddo
            enddo
         enddo
      endif

      if (index_o2x_Faoo_fbrf_ocn > 0) then
         do j = 1, jj
            do l = 1, isp(j)
            do i = max(1,ifp(j,l)), min(ii,ilp(j,l))
               sbuff(i,j,index_o2x_Faoo_fbrf_ocn) =
     .            sbuff(i,j,index_o2x_Faoo_fbrf_ocn)
     .          + flxbrf(i,j)*baclin
            enddo
            enddo
         enddo
      endif

      if (index_o2x_Faoo_fn2o_ocn > 0) then
         do j = 1, jj
            do l = 1, isp(j)
            do i = max(1,ifp(j,l)), min(ii,ilp(j,l))
               sbuff(i,j,index_o2x_Faoo_fn2o_ocn) =
     .            sbuff(i,j,index_o2x_Faoo_fn2o_ocn)
     .          + flxn2o(i,j)*baclin
            enddo
            enddo
         enddo
      endif

      if (index_o2x_Faoo_fnh3_ocn > 0) then
         do j = 1, jj
            do l = 1, isp(j)
            do i = max(1,ifp(j,l)), min(ii,ilp(j,l))
               sbuff(i,j,index_o2x_Faoo_fnh3_ocn) =
     .            sbuff(i,j,index_o2x_Faoo_fnh3_ocn)
     .          + flxnh3(i,j)*baclin
            enddo
            enddo
         enddo
      endif



      !-----------------------------------------------------------------
      ! Increment time since last coupling
      !-----------------------------------------------------------------

      tlast_coupled = tlast_coupled + baclin

      end subroutine sumsbuff_mct
