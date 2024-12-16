include(FetchContent)
find_package(OpenMP REQUIRED)

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=swap")

FetchContent_Declare(fiat
    GIT_REPOSITORY https://github.com/ecmwf-ifs/fiat.git
    GIT_TAG 1.4.1
    GIT_SHALLOW TRUE
    GIT_PROGRESS TRUE
)
FetchContent_MakeAvailable(fiat)

add_library(struct_mod STATIC)

target_sources(struct_mod
    PUBLIC
./common/micro/modd_cloudparn.F90
./common/micro/modd_elec_descr.F90
./common/micro/modd_elec_param.F90
./common/micro/modd_elecn.F90
./common/micro/modd_fields_address.F90
./common/micro/modd_nebn.F90
./common/micro/modd_param_icen.F90
./common/micro/modd_param_lima.F90
./common/micro/modd_param_lima_cold.F90
./common/micro/modd_param_lima_mixed.F90
./common/micro/modd_param_lima_warm.F90
./common/micro/modd_rain_ice_descrn.F90
./common/micro/modd_rain_ice_paramn.F90
./common/aux/modd_budget.F90
./common/aux/modd_conf.F90
./common/aux/modd_cst.F90
./common/aux/modd_dimphyexn.F90
./common/aux/modd_field.F90
./common/aux/modd_io.F90
./common/aux/modd_les.F90
./common/aux/modd_nsv.F90
./common/aux/modd_parameters.F90
./common/aux/modd_phyex.F90
./common/aux/modd_precision.F90
./common/aux/modd_argslist_ll.F90
./common/micro/modd_tiwmx.F90
)

target_link_libraries(struct_mod
    PRIVATE
        fiat
        parkind_dp
        )


add_library(common STATIC)


target_sources(common
    PRIVATE
./common/aux/modd_misc.F90
./common/aux/gamma.F90
./common/aux/gamma_inc.F90
./common/aux/general_gamma.F90
./common/aux/get_halo.F90
./common/aux/gradient_m.F90
./common/aux/gradient_u.F90
./common/aux/gradient_v.F90
./common/aux/gradient_w.F90
./common/aux/ini_phyex.F90
./common/aux/mode_argslist_ll_phy.F90
./common/aux/mode_budget_phy.F90
./common/aux/mode_check_nam_val.F90
./common/aux/mode_gradient_m_phy.F90
./common/aux/mode_gradient_u_phy.F90
./common/aux/mode_gradient_v_phy.F90
./common/aux/mode_gradient_w_phy.F90
./common/aux/mode_ini_cst.F90
./common/micro/mode_ini_cloudpar.F90
./common/aux/mode_io_field_write.F90
./common/aux/mode_io_field_write_phy.F90
./common/aux/mode_ll.F90
./common/aux/mode_mppdb.F90
./common/aux/mode_msg.F90
./common/aux/mode_posnam_phy.F90
./common/aux/mode_shuman_phy.F90
./common/aux/mode_sources_neg_correct.F90
./common/aux/mode_thermo.F90
./common/aux/modi_gamma.F90
./common/aux/modi_gamma_inc.F90
./common/aux/modi_general_gamma.F90
./common/aux/modi_gradient_m.F90
./common/aux/modi_gradient_u.F90
./common/aux/modi_gradient_v.F90
./common/aux/modi_gradient_w.F90
./common/aux/modi_ini_phyex.F90
./common/aux/modi_second_mnh.F90
./common/aux/modi_shuman.F90
./common/aux/second_mnh.F90
./common/aux/shuman.F90
./common/aux/tools.F90
./common/micro/condensation.F90
./common/micro/hypgeo.F90
./common/micro/hypser.F90
./common/micro/ice_adjust.F90
./common/micro/lima.F90
./common/micro/lima_adjust_split.F90
./common/micro/lima_precip_scavenging.F90
./common/micro/minpack.F90
./common/micro/mode_compute_lambda.F90
./common/micro/mode_elec_beard_effect.F90
./common/micro/mode_elec_compute_ex.F90
./common/micro/mode_elec_tendencies.F90
./common/micro/mode_ice4_budgets.F90
./common/micro/mode_ice4_compute_pdf.F90
./common/micro/mode_ice4_correct_negativities.F90
./common/micro/mode_ice4_fast_rg.F90
./common/micro/mode_ice4_fast_rh.F90
./common/micro/mode_ice4_fast_ri.F90
./common/micro/mode_ice4_fast_rs.F90
./common/micro/mode_ice4_pack.F90
./common/micro/mode_ice4_rainfr_vert.F90
./common/micro/mode_ice4_rimltc.F90
./common/micro/mode_ice4_rrhong.F90
./common/micro/mode_ice4_rsrimcg_old.F90
./common/micro/mode_ice4_sedimentation.F90
./common/micro/mode_ice4_sedimentation_split.F90
./common/micro/mode_ice4_sedimentation_stat.F90
./common/micro/mode_ice4_slow.F90
./common/micro/mode_ice4_stepping.F90
./common/micro/mode_ice4_tendencies.F90
./common/micro/mode_ice4_warm.F90
./common/micro/mode_icecloud.F90
./common/micro/mode_ini_lima.F90
./common/micro/mode_ini_lima_cold_mixed.F90
./common/micro/mode_ini_lima_warm.F90
./common/micro/mode_ini_rain_ice.F90
./common/micro/mode_ini_snow.F90
./common/micro/mode_ini_tiwmx.F90
./common/micro/mode_init_aerosol_properties.F90
./common/micro/mode_lima_bergeron.F90
./common/micro/mode_lima_ccn_activation.F90
./common/micro/mode_lima_ccn_hom_freezing.F90
./common/micro/mode_lima_collisional_ice_breakup.F90
./common/micro/mode_lima_compute_cloud_fractions.F90
./common/micro/mode_lima_conversion_melting_snow.F90
./common/micro/mode_lima_droplets_accretion.F90
./common/micro/mode_lima_droplets_autoconversion.F90
./common/micro/mode_lima_droplets_hom_freezing.F90
./common/micro/mode_lima_droplets_riming_snow.F90
./common/micro/mode_lima_droplets_self_collection.F90
./common/micro/mode_lima_drops_break_up.F90
./common/micro/mode_lima_drops_hom_freezing.F90
./common/micro/mode_lima_drops_self_collection.F90
./common/micro/mode_lima_drops_to_droplets_conv.F90
./common/micro/mode_lima_functions.F90
./common/micro/mode_lima_graupel.F90
./common/micro/mode_lima_graupel_deposition.F90
./common/micro/mode_lima_hail.F90
./common/micro/mode_lima_hail_deposition.F90
./common/micro/mode_lima_ice4_nucleation.F90
./common/micro/mode_lima_ice_aggregation_snow.F90
./common/micro/mode_lima_ice_deposition.F90
./common/micro/mode_lima_ice_melting.F90
./common/micro/mode_lima_init_ccn_activation_spectrum.F90
./common/micro/mode_lima_inst_procs.F90
./common/micro/mode_lima_meyers_nucleation.F90
./common/micro/mode_lima_nucleation_procs.F90
./common/micro/mode_lima_phillips_ifn_nucleation.F90
./common/micro/mode_lima_phillips_integ.F90
./common/micro/mode_lima_phillips_ref_spectrum.F90
./common/micro/mode_lima_rain_accr_snow.F90
./common/micro/mode_lima_rain_evaporation.F90
./common/micro/mode_lima_rain_freezing.F90
./common/micro/mode_lima_raindrop_shattering_freezing.F90
./common/micro/mode_lima_read_xker_gweth.F90
./common/micro/mode_lima_read_xker_raccs.F90
./common/micro/mode_lima_read_xker_rdryg.F90
./common/micro/mode_lima_read_xker_sdryg.F90
./common/micro/mode_lima_read_xker_sweth.F90
./common/micro/mode_lima_sedimentation.F90
./common/micro/mode_lima_snow_deposition.F90
./common/micro/mode_lima_snow_self_collection.F90
./common/micro/mode_lima_tendencies.F90
./common/micro/mode_lima_update_nsv.F90
./common/micro/mode_nrcolss.F90
./common/micro/mode_nscolrg.F90
./common/micro/mode_nzcolx.F90
./common/micro/mode_qsatmx_tab.F90
./common/micro/mode_rain_ice_old_fast_rg.F90
./common/micro/mode_rain_ice_old_fast_rh.F90
./common/micro/mode_rain_ice_old_fast_ri.F90
./common/micro/mode_rain_ice_old_fast_rs.F90
./common/micro/mode_rain_ice_old_icenumber2.F90
./common/micro/mode_rain_ice_old_nucleation.F90
./common/micro/mode_rain_ice_old_sedimentation_split.F90
./common/micro/mode_rain_ice_old_sedimentation_stat.F90
./common/micro/mode_rain_ice_old_slow.F90
./common/micro/mode_rain_ice_old_warm.F90
./common/micro/mode_read_xker_gweth.F90
./common/micro/mode_read_xker_raccs.F90
./common/micro/mode_read_xker_rdryg.F90
./common/micro/mode_read_xker_rweth.F90
./common/micro/mode_read_xker_sdryg.F90
./common/micro/mode_read_xker_sweth.F90
./common/micro/mode_rrcolss.F90
./common/micro/mode_rscolrg.F90
./common/micro/mode_rzcolx.F90
./common/micro/mode_set_conc_lima.F90
./common/micro/mode_tiwmx.F90
./common/micro/mode_tiwmx_fun.F90
./common/micro/mode_tiwmx_tab.F90
./common/micro/modi_condensation.F90
./common/micro/modi_ice_adjust.F90
./common/micro/modi_lima.F90
./common/micro/modi_lima_adjust_split.F90
./common/micro/modi_lima_precip_scavenging.F90
./common/micro/modi_minpack.F90
./common/micro/modi_rain_ice.F90
./common/micro/modi_rain_ice_old.F90
./common/micro/momg.F90
./common/micro/rain_ice.F90
./common/micro/rain_ice_old.F90
./common/turb/les_mean_subgrid.F90
./common/turb/les_mean_subgrid_phy.F90
./common/turb/modd_cturb.F90
./common/turb/modd_param_mfshalln.F90
./common/turb/modd_turbn.F90
./common/turb/mode_bl89.F90
./common/turb/mode_bl_depth_diag.F90
./common/turb/mode_coefj.F90
./common/turb/mode_compute_bl89_ml.F90
./common/turb/mode_compute_function_thermo_mf.F90
./common/turb/mode_compute_mf_cloud.F90
./common/turb/mode_compute_mf_cloud_bigaus.F90
./common/turb/mode_compute_mf_cloud_direct.F90
./common/turb/mode_compute_mf_cloud_stat.F90
./common/turb/mode_compute_updraft.F90
./common/turb/mode_compute_updraft_raha.F90
./common/turb/mode_compute_updraft_rhcj10.F90
./common/turb/mode_emoist.F90
./common/turb/mode_etheta.F90
./common/turb/mode_ibm_mixinglength.F90
./common/turb/mode_ini_mfshall.F90
./common/turb/mode_ini_turb.F90
./common/turb/mode_mf_turb.F90
./common/turb/mode_mf_turb_expl.F90
./common/turb/mode_prandtl.F90
./common/turb/mode_rmc01.F90
./common/turb/mode_rotate_wind.F90
./common/turb/mode_sbl.F90
./common/turb/mode_sbl_depth.F90
./common/turb/mode_sbl_phy.F90
./common/turb/mode_thl_rt_from_th_r_mf.F90
./common/turb/mode_tke_eps_sources.F90
./common/turb/mode_tm06.F90
./common/turb/mode_tm06_h.F90
./common/turb/mode_tridiag.F90
./common/turb/mode_tridiag_massflux.F90
./common/turb/mode_tridiag_thermo.F90
./common/turb/mode_tridiag_tke.F90
./common/turb/mode_tridiag_w.F90
./common/turb/mode_tridiag_wind.F90
./common/turb/mode_turb_hor.F90
./common/turb/mode_turb_hor_dyn_corr.F90
./common/turb/mode_turb_hor_splt.F90
./common/turb/mode_turb_hor_sv_corr.F90
./common/turb/mode_turb_hor_sv_flux.F90
./common/turb/mode_turb_hor_thermo_corr.F90
./common/turb/mode_turb_hor_thermo_flux.F90
./common/turb/mode_turb_hor_tke.F90
./common/turb/mode_turb_hor_uv.F90
./common/turb/mode_turb_hor_uw.F90
./common/turb/mode_turb_hor_vw.F90
./common/turb/mode_turb_ver.F90
./common/turb/mode_turb_ver_dyn_flux.F90
./common/turb/mode_turb_ver_sv_corr.F90
./common/turb/mode_turb_ver_sv_flux.F90
./common/turb/mode_turb_ver_thermo_corr.F90
./common/turb/mode_turb_ver_thermo_flux.F90
./common/turb/mode_update_iiju_phy.F90
./common/turb/mode_update_lm.F90
./common/turb/modi_shallow_mf.F90
./common/turb/modi_turb.F90
./common/turb/shallow_mf.F90
./common/turb/shuman_mf.F90
./common/turb/turb.F90
./offline/support/mode_mnh_zwork.F90
)

target_include_directories( common PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/common/micro>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/common/turb>
)

target_link_libraries(common
    PRIVATE
        fiat
        parkind_dp
    PUBLIC
        struct_mod
        )



add_library(exe_support STATIC)


target_sources(exe_support 
    PRIVATE
./offline/progs/getdata_ice_adjust_mod.F90
# ./offline/progs/getdata_lima_adjust_mod.F90
# ./offline/progs/getdata_lima_mod.F90
# ./offline/progs/getdata_rain_ice_mod.F90
# ./offline/progs/getdata_rain_ice_old_mod.F90
# ./offline/progs/getdata_shallow_mod.F90
# ./offline/progs/getdata_turb_mod.F90
./offline/support/arrays_manip.F90
./offline/support/compute_diff.F90
./offline/support/stack_mod.F90
./offline/support/xrd_getoptions.F90
./offline/support/xrd_unix_env.F90
)

target_link_libraries(exe_support
    PRIVATE
        fiat
        parkind_dp
    PUBLIC
        struct_mod
        )

add_executable(main_ice_adjust offline/progs/main_ice_adjust.F90)

target_link_libraries(main_ice_adjust 
    PRIVATE
    parkind_dp
    common
    fiat 
    exe_support
    OpenMP::OpenMP_Fortran
    )

target_link_libraries(main_ice_adjust
    PUBLIC 
    struct_mod
)
