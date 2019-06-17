program wwlln_de

! gfortran -o wwlln_de wwlln_de.f90 -I/usr/local/include -L/usr/local/lib -lnetcdff -ludunits

use iso_fortran_env, only : real64
use netcdf
use calctimemod

implicit none

character(100) :: infile
character(100) :: outfile

real, allocatable, dimension(:,:,:) :: de

real, dimension(180) :: line = -9999.

character(8) :: datestring

integer :: nrows
integer :: ncols
integer :: nhrs

integer :: h,i

integer :: status
integer :: ofid
integer :: varid

integer :: t

real :: tvect

integer :: year
integer :: month
integer :: day
integer :: hour
integer :: min
real    :: sec

integer :: yearout
integer :: monout

real(dp), dimension(24) :: timereal

character(40) :: timeref

integer :: pos

!--------------------------------------------

call udunitsinit()

!------------------------------

call getarg(1,infile)

open(10,file=infile,status='old')

read(10,*)line

nrows = count(line /= -9999.)

ncols = 2 * nrows
nhrs = 24

if (nrows /= 180) then
  write(*,'(a,i5,a)')'ERROR input array has',nrows,', I need 180 rows'
  stop 
end if

rewind(10)

allocate(de(ncols,nrows,nhrs))

pos = scan(infile,'DE2',back=.true.)

write(*,*)infile(pos:pos+7)

read(infile(pos:pos+3),*)year
read(infile(pos+4:pos+5),*)month
read(infile(pos+6:pos+7),*)day

write(0,*)year,month,day

!------------------------------

call getarg(2,outfile)

status = nf90_open(outfile,nf90_write,ofid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inq_varid(ofid,'time',varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_att(ofid,varid,'units',timeref)
if (status /= nf90_noerr) call handle_err(status)

write(*,*)timeref

call udsettime(timeref)

!------------------------------

write(*,*)'reading'

min = 0
sec = 0.0

do h = 1,nhrs
  do i = 1,ncols
    read(10,*,end=99)de(i,:,h)
  end do
  read(10,*) ! blank separator line

  hour = h - 1
  
  call calctime(year,month,day,hour,min,sec,timereal(h))

!   write(0,*)year,month,day,hour,min,sec,timereal(h)
  
end do

99 close(10)

!------------------------------

write(*,*)'writing'

t = 1 + int(timereal(1))

status = nf90_inq_varid(ofid,'time',varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_put_var(ofid,varid,timereal,start=[t],count=[nhrs])
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inq_varid(ofid,'de',varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_put_var(ofid,varid,de,start=[1,1,t],count=[ncols,nrows,nhrs])
if (status /= nf90_noerr) call handle_err(status)

status = nf90_close(ofid)
if (status /= nf90_noerr) call handle_err(status)

call udunitsend()

contains

!--------------------------------------------------------------------------------

subroutine handle_err(status)

implicit none

! Internal subroutine - checks error status after each netcdf call,
! prints out text message each time an error code is returned. 

integer, intent (in) :: status

if(status /= nf90_noerr) then 
  write(0,*)trim(nf90_strerror(status))
  stop
end if

end subroutine handle_err

!--------------------------------------------------------------------------------

end program wwlln_de