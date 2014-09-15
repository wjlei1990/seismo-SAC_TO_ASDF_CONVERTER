!> @file
!> containts only the main program for SAC_TO_ASDF_CONVERTER

!! @brief Generates adios .bp files from SAC files 
!! @author Wenjie Lei, James Smith
!! @warning 1) only works on 1 processor. not parallel
!! 2) can only convert SAC files from one event

program sac_to_asdf

    use seismo_constants
    use main_parameter
    use asdf_data
    use main_subs
    use asdf_read_subs
    use asdf_write_subs
    use mpi
    implicit none

    type(asdf_event) :: asdf_container

    integer             :: comm, rank, nproc
    integer             :: ierr, i, adios_err
    integer             :: n_records
    integer(kind=8)     :: adios_group

    character(len=13)   :: event_name
    character(len=200)  :: sac_file_list(N_RECORDS_MAX)
    character(len=200)  :: ASDF_FN

    ! init mpi

    call MPI_Init (ierr)
    call MPI_Comm_dup (MPI_COMM_WORLD, comm, ierr)
    call MPI_Comm_rank (comm, rank, ierr)
    call MPI_Comm_size (comm, nproc, ierr)

    !init adios
    call adios_init_noxml (comm, adios_err)
    call adios_allocate_buffer (600, adios_err)
    call adios_declare_group (adios_group, "EVENTS", "iter", 1, adios_err)
    call adios_select_method (adios_group, "MPI", "", "", adios_err)

    !read parameter file
    call read_parameter_file(event_name)

    !generate the data file list
    call generate_filelist(sac_file_list, n_records)

    !init asdf data structure
    call init_asdf_data(asdf_container, n_records, STORE_RESPONSE)

    !copy the data into asdf structure
    call copy_sac_to_asdf_data(asdf_container, sac_file_list, n_records, event_name)

    !write out
    print *,"===================="
    print *,"Begin write out"

    call system('mkdir -p '//trim(OUTDIR)//'')

    ASDF_FN=trim(OUTDIR)//'/'//trim(event_name)//"_"//trim(SAC_FILE_SUFFIX)//".bp"

    print *,"n_records to be written out: ", asdf_container%nrecords
    if(asdf_container%nrecords.ge.1)then
      call write_asdf_file (ASDF_FN, asdf_container, adios_group, rank, nproc, comm, ierr)
    else
      print *,"No records...Stop the program"
    endif

    call MPI_Barrier (comm, ierr)
    call adios_finalize(rank, ierr)
    call MPI_Finalize (ierr)

end program
