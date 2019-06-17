module calctimemod

!link with -ludunits

implicit none

public  :: udunitsinit
public  :: udsettime
public  :: calctime
public  :: udunitsend
private :: uderr
private :: status

integer, parameter :: i8 = selected_int_kind(13)
integer, parameter :: dp = selected_real_kind(13)

character(80), parameter :: unitsfile = '/usr/local/etc/udunits.dat'

integer :: status

integer(i8) :: utu  !reference time pointer

contains

!------------------------------------------------------

subroutine udunitsinit()

implicit none

integer :: utopen

!-----

status = utopen(unitsfile)
if (status /= 0) call uderr(status)

end subroutine udunitsinit

!------------------------------------------------------

subroutine udsettime(timeref)

implicit none

character(*), intent(in) :: timeref

integer(i8) :: utmake
integer     :: utdec

!-----

utu = utmake()

status = utdec(timeref,utu)
if (status /= 0) call uderr(status)

end subroutine udsettime

!------------------------------------------------------

subroutine udunitsend()

implicit none

call utcls()

end subroutine udunitsend

!------------------------------------------------------

subroutine calctime(year,month,day,hour,min,sec,timereal)

integer, intent(in) :: year
integer, intent(in) :: month
integer, intent(in) :: day
integer, intent(in) :: hour
integer, intent(in) :: min
real,    intent(in) :: sec

real(dp), intent(out) :: timereal

integer :: uticaltime

!-------------

status = uticaltime(year,month,day,hour,min,sec,utu,timereal)
if (status /= 0) call uderr(status)

end subroutine calctime

!------------------------------------------------------

subroutine getdate(timereal,year,month,day,hour,min,sec)

real(dp), intent(in) :: timereal

integer, intent(out) :: year
integer, intent(out) :: month
integer, intent(out) :: day
integer, intent(out) :: hour
integer, intent(out) :: min
real,    intent(out) :: sec

integer :: utcaltime

!-------------

status = utcaltime(timereal,utu,year,month,day,hour,min,sec)
if (status /= 0) call uderr(status)

end subroutine getdate

!------------------------------------------------------

subroutine uderr(status)

integer, intent(in) :: status

write(0,*)'udunits error:',status
stop

end subroutine uderr

!------------------------------------------------------

end module calctimemod
