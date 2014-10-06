module seismo_constants

  integer, parameter :: NDATAMAX=1000000
  integer, parameter :: N_RECORDS_MAX=20000

end module seismo_constants

module main_parameter

  logical :: DEBUG

  character(len=150) :: SAC_FILE_DIR, OUTDIR
  character(len=32)  :: SAC_FILE_SUFFIX
  
  logical :: STORE_RESPONSE

end module main_parameter

module source_info

  type cmt_struct
    integer :: gmt_year, gmt_month, gmt_day
    integer :: gmt_hour, gmt_min
    real :: gmt_sec
    real :: Mb, Ms
    real :: time_shift, half_duration
    real :: latitude, longitude, depth
    real :: Mrr, Mtt, Mpp, Mrt, Mrp, Mtp
    character(len=100) :: PDE_event_name
    character(len=13)  :: event_name
    character(len=20)  :: datasource
  end type cmt_struct

end module source_info

module seismo_variables

  integer :: gmt_year
  integer :: gmt_hour
  integer :: gmt_day
  integer :: gmt_min
  integer :: gmt_sec
  integer :: gmt_msec

  real :: event_lat, event_lo, event_dpt
  real :: receiver_lat, receiver_lo, receiver_el, receiver_dpt
  real :: end_value, P_pick, S_pick, cmp_azimuth, cmp_incident_ang
  real :: scale_factor, ev_to_sta_AZ, sta_to_ev_AZ, great_circle_arc, dist

end module seismo_variables
