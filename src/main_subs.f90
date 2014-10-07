!> @file
!! contains the subroutine for the main program of SAC_TO_ASDF_CONVERTER

module main_subs

  implicit none

contains

subroutine read_parameter_file(event_name)

  use main_parameter
  !use asdf_data,only:STORE_RESPONSE
  implicit none
  
  character(len=*) :: event_name

  integer, parameter :: IIN = 11
  integer, parameter :: NHEAD = 8

  integer :: idummy
  character(len=28) :: junk

  open(unit=IIN,file='PAR_FILE',status='old')

  print *,"==========================="
  print *,"Reading the parameter file for main program."

  ! ignore header
  do idummy=1,NHEAD
    read(IIN,*)
  enddo

  read(IIN,3) junk, DEBUG
  read(IIN,*)
  read(IIN,*)
  read(IIN,2) junk, event_name
  read(IIN,*) 
  read(IIN,*)
  read(IIN,2) junk, SAC_FILE_DIR
  read(IIN,2) junk, SAC_FILE_SUFFIX
  read(IIN,3) junk, STORE_RESPONSE
  read(IIN,*) 
  read(IIN,*) 
  read(IIN,2) junk, OUTDIR

  close(IIN)
  
  print *,"DEBUG:", DEBUG
  if(DEBUG) then
    print *, "Event name: ", trim(event_name)
    print *, "SAC_FILE_DIR: ", trim(SAC_FILE_DIR)
    print *, "SAC_FILE_SUFFIX: ", trim(SAC_FILE_SUFFIX)
    print *, "OUTDIR: ", trim(OUTDIR)
  endif

2 format(a,a)
3 format(a,l20)

end subroutine read_parameter_file

subroutine generate_filelist(fn_list, n_records)

  use main_parameter

  character(len=*) :: fn_list(:)
  integer,intent(inout) :: n_records
  integer :: ierr
  character(len=300) :: command

  print *, "====================="
  print *, "Generate sac file list"

  command = "ls "//trim(SAC_FILE_DIR)//"/*."//trim(SAC_FILE_SUFFIX)//" > file_list"
  call system(command)

  ierr=0
  n_records = 0

  open(unit=51,file="file_list")
  do while(ierr.eq.0)
    read(51,'(a)',iostat=ierr) fn_list(n_records+1)
    if(ierr.eq.0) n_records=n_records+1
  enddo
  close(51)

end subroutine generate_filelist

!> @brief copy the sac information to asdf data structure
subroutine copy_sac_to_asdf_data(asdf_container, fn_list, n_records, event_name)

  use asdf_data
  use seismo_constants
  use main_parameter
  use seismo_variables

  type(asdf_event) :: asdf_container
  character(len=*) :: fn_list(:)
  character(len=*) :: event_name
  integer :: n_records

  real :: B, DELTA
  integer :: npoints
  real, dimension(NDATAMAX) :: displ

  character(len=300) :: command
  character(len=200) :: fn, response_fn
  character(len=20) :: kstnm, kcmpnm, knetwk, khole

  integer :: i
  integer :: asdf_index
  logical :: file_exists

  !container var
  character(len=:), allocatable :: receiver_name, network
  character(len=:), allocatable :: component, receiver_id 
  character(len=:), allocatable :: response

  integer :: nerr
  integer :: nerr_files=0
  integer :: response_length

  allocate(character(len=6*3*n_records)::receiver_name)
  allocate(character(len=6*3*n_records)::network)
  allocate(character(len=6*3*n_records)::component)
  allocate(character(len=6*3*n_records)::receiver_id)

  print *, "====================="
  print *, "Copy the sac info to asdf structure"

  receiver_name=""
  network=""
  component=""
  receiver_id=""

  asdf_index=0
  nerr_files=0

  do i=1, n_records
    fn = fn_list(i)
    print *,"Read in fn:", trim(fn)
    call rsac1(trim(adjustl(fn)), displ, npoints, B, DELTA, &
             NDATAMAX, nerr)

    !if error reading sac file, skip it
    if(nerr.ne.0)then
      !Read in sac file fails...
      print *, "Error read in sac file: ", trim(fn)
      !write(110,*) "Error"
      nerr_files=nerr_files+1
      cycle
    endif

    !if error reading station name, skip it
    call getkhv('kstnm', kstnm, nerr)
    if(nerr.ne.0)then
      print *,"SAC file contaminated(receiver_name)...skip this data"
      cycle
    endif

    !if error reading component name, skip it
    call getkhv('kcmpnm', kcmpnm(1:10), nerr)
    if(nerr.ne.0)then
      print *,"SAC file contaminated(kcmpnm)...skip this data"
      cycle
    endif

    !if error reading network name, skip it
    call getkhv('knetwk', knetwk, nerr)
    if(nerr.ne.0)then
      print *,"SAC file contaminated(network)...skip this data"
      cycle
    endif
    
    asdf_index=asdf_index+1
    allocate(asdf_container%records(asdf_index)%record(npoints))
    asdf_container%npoints(asdf_index) = npoints
    asdf_container%records(asdf_index)%record(1:npoints) = displ(1:npoints)
    asdf_container%sample_rate(asdf_index) = DELTA
    asdf_container%begin_value(asdf_index) = B

    print *,"(Notes: Error may just mean the slot in SAC header is not initilized )"

    asdf_container%receiver_name_array(asdf_index)=kstnm
    asdf_container%network_array(asdf_index)=knetwk
    !manually modify kcmpnm here
    !print *, "comp:", trim(kcmpnm(1:3))
    kcmpnm="LH"//trim(kcmpnm(3:3))
    asdf_container%component_array(asdf_index)=kcmpnm
    !print *, "comp:", trim(kcmpnm)

    call getkhv('khole', khole, nerr)
    if(nerr.ne.0)then
      print *,"khole missing, set it to null string."
      asdf_container%receiver_id_array(asdf_index)=""
    else
      asdf_container%receiver_id_array(asdf_index)=khole
    endif

    call getnhv('NZYEAR', asdf_container%gmt_year(asdf_index), nerr)
    call getnhv('NZJDAY', asdf_container%gmt_day(asdf_index), nerr)
    call getnhv('NZHOUR', asdf_container%gmt_hour(asdf_index), nerr)
    call getnhv('NZMIN', asdf_container%gmt_min(asdf_index), nerr)
    call getnhv('NZSEC', asdf_container%gmt_sec(asdf_index), nerr)
    call getnhv('NZMSEC', asdf_container%gmt_msec(asdf_index), nerr)

    call getfhv('EVLA',event_lat, nerr)
    call getfhv('EVLO',event_lo, nerr)
    call getfhv('EVDP',event_dpt, nerr)

    call getfhv('STLA', receiver_lat, nerr)
    call getfhv('STLO', receiver_lo, nerr)
    call getfhv('STEL', receiver_el, nerr)
    call getfhv('STDP', receiver_dpt, nerr)

    call getfhv('E', end_value, nerr)

    call getfhv('t1',P_pick,nerr)
    call getfhv('t2',S_pick,nerr)
    call getfhv('CMPAZ', cmp_azimuth, nerr)
    call getfhv('CMPINC', cmp_incident_ang, nerr)
    call getfhv('SCALE', scale_factor, nerr)

    call getfhv('AZ',ev_to_sta_AZ,nerr)
    call getfhv('BAZ',sta_to_ev_AZ,nerr)
    call getfhv('GCARC',great_circle_arc,nerr)
    call getfhv('DIST',dist,nerr)

    if (STORE_RESPONSE) then
      response_fn = trim(SAC_FILE_DIR)//"/RESP."//trim(knetwk)//&
                "."//trim(kstnm)//".."//trim(kcmpnm)
      inquire(file=trim(response_fn), exist=file_exists)
      if (.not.file_exists) then
        response_fn = ""
        command = "ls "//trim(SAC_FILE_DIR)//"/RESP*."//trim(knetwk)//"."&
                //trim(kstnm)//".*."//trim(kcmpnm)//" > response_file"
        call system(command)
        open(17, file="response_file", status='old')
        read(17, 2) response_fn
        close(17)
        2 format(a,a)
        inquire(file=trim(response_fn), exist=file_exists)
        if (.not.file_exists) then
          print *, "No response found for station ", trim(kstnm), &
            ", network ", trim(knetwk), " and component ", trim(kcmpnm)
          stop
        endif
      endif
      inquire(file=trim(response_fn), size=response_length)
      asdf_container%responses(asdf_index)%response_length=response_length
      open(21, file=trim(response_fn), status='old', action='read', &
          recl=response_length, &
          form='unformatted', access='direct')
      allocate(character(len=response_length)::response)
      read(21, rec=1) response
      asdf_container%responses(asdf_index)%response_string = trim(response)
      deallocate(response)
      close(21)
    endif

    !asdf_container%gmt_year(asdf_index) = gmt_year
    !asdf_container%gmt_day(asdf_index) = gmt_day
    !asdf_container%gmt_hour(asdf_index) = gmt_hour
    !asdf_container%gmt_min(asdf_index) = gmt_min
    !asdf_container%gmt_sec(asdf_index) = gmt_sec
    !asdf_container%gmt_msec(asdf_index) = gmt_msec

    asdf_container%event_lat(asdf_index) = dble(event_lat)
    asdf_container%event_lo(asdf_index) = dble(event_lo)
    asdf_container%event_dpt(asdf_index) = dble(event_dpt)

    asdf_container%receiver_lat(asdf_index)=dble(receiver_lat)
    asdf_container%receiver_lo(asdf_index)=dble(receiver_lo)
    asdf_container%receiver_el(asdf_index)=dble(receiver_el)
    asdf_container%receiver_dpt(asdf_index)=dble(receiver_dpt)

    asdf_container%end_value(asdf_index)=dble(end_value)
    asdf_container%P_pick(asdf_index)=dble(P_pick)
    asdf_container%S_pick(asdf_index)=dble(S_pick)
    asdf_container%cmp_azimuth(asdf_index)=dble(cmp_azimuth)
    asdf_container%cmp_incident_ang(asdf_index)=dble(cmp_incident_ang)
    asdf_container%scale_factor(asdf_index)=dble(scale_factor)

    asdf_container%ev_to_sta_AZ(asdf_index)=dble(ev_to_sta_AZ)
    asdf_container%sta_to_ev_AZ(asdf_index)=dble(sta_to_ev_AZ)
    asdf_container%great_circle_arc(asdf_index)=dble(great_circle_arc)
    asdf_container%dist(asdf_index)=dble(dist)

  enddo

  asdf_container%nrecords=asdf_index
  asdf_container%event=event_name

end subroutine copy_sac_to_asdf_data

subroutine copy_cmt_to_asdf_data(asdf_container, cmt)

  use source_info
  use asdf_data

  type(cmt_struct), intent(in) :: cmt 
  type(asdf_event) :: asdf_container

  asdf_container%event_lat = cmt%latitude
  asdf_container%event_lo  = cmt%longitude
  asdf_container%event_dpt = cmt%depth

end subroutine copy_cmt_to_asdf_data

end module main_subs
