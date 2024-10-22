// Generated by cpp11: do not edit by hand
// clang-format off


#include "cpp11/declarations.hpp"
#include <R_ext/Visibility.h>

// cows.cpp
SEXP dust2_system_cows_alloc(cpp11::list r_pars, cpp11::sexp r_time, cpp11::list r_time_control, cpp11::sexp r_n_particles, cpp11::sexp r_n_groups, cpp11::sexp r_seed, cpp11::sexp r_deterministic, cpp11::sexp r_n_threads);
extern "C" SEXP _cowflu_dust2_system_cows_alloc(SEXP r_pars, SEXP r_time, SEXP r_time_control, SEXP r_n_particles, SEXP r_n_groups, SEXP r_seed, SEXP r_deterministic, SEXP r_n_threads) {
  BEGIN_CPP11
    return cpp11::as_sexp(dust2_system_cows_alloc(cpp11::as_cpp<cpp11::decay_t<cpp11::list>>(r_pars), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_time), cpp11::as_cpp<cpp11::decay_t<cpp11::list>>(r_time_control), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_n_particles), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_n_groups), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_seed), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_deterministic), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_n_threads)));
  END_CPP11
}
// cows.cpp
SEXP dust2_system_cows_run_to_time(cpp11::sexp ptr, cpp11::sexp r_time);
extern "C" SEXP _cowflu_dust2_system_cows_run_to_time(SEXP ptr, SEXP r_time) {
  BEGIN_CPP11
    return cpp11::as_sexp(dust2_system_cows_run_to_time(cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(ptr), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_time)));
  END_CPP11
}
// cows.cpp
SEXP dust2_system_cows_state(cpp11::sexp ptr, cpp11::sexp r_index_state, cpp11::sexp r_index_particle, cpp11::sexp r_index_group, bool preserve_particle_dimension, bool preserve_group_dimension);
extern "C" SEXP _cowflu_dust2_system_cows_state(SEXP ptr, SEXP r_index_state, SEXP r_index_particle, SEXP r_index_group, SEXP preserve_particle_dimension, SEXP preserve_group_dimension) {
  BEGIN_CPP11
    return cpp11::as_sexp(dust2_system_cows_state(cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(ptr), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_index_state), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_index_particle), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_index_group), cpp11::as_cpp<cpp11::decay_t<bool>>(preserve_particle_dimension), cpp11::as_cpp<cpp11::decay_t<bool>>(preserve_group_dimension)));
  END_CPP11
}
// cows.cpp
SEXP dust2_system_cows_time(cpp11::sexp ptr);
extern "C" SEXP _cowflu_dust2_system_cows_time(SEXP ptr) {
  BEGIN_CPP11
    return cpp11::as_sexp(dust2_system_cows_time(cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(ptr)));
  END_CPP11
}
// cows.cpp
SEXP dust2_system_cows_set_state_initial(cpp11::sexp ptr);
extern "C" SEXP _cowflu_dust2_system_cows_set_state_initial(SEXP ptr) {
  BEGIN_CPP11
    return cpp11::as_sexp(dust2_system_cows_set_state_initial(cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(ptr)));
  END_CPP11
}
// cows.cpp
SEXP dust2_system_cows_set_state(cpp11::sexp ptr, cpp11::list r_state);
extern "C" SEXP _cowflu_dust2_system_cows_set_state(SEXP ptr, SEXP r_state) {
  BEGIN_CPP11
    return cpp11::as_sexp(dust2_system_cows_set_state(cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(ptr), cpp11::as_cpp<cpp11::decay_t<cpp11::list>>(r_state)));
  END_CPP11
}
// cows.cpp
SEXP dust2_system_cows_reorder(cpp11::sexp ptr, cpp11::integers r_index);
extern "C" SEXP _cowflu_dust2_system_cows_reorder(SEXP ptr, SEXP r_index) {
  BEGIN_CPP11
    return cpp11::as_sexp(dust2_system_cows_reorder(cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(ptr), cpp11::as_cpp<cpp11::decay_t<cpp11::integers>>(r_index)));
  END_CPP11
}
// cows.cpp
SEXP dust2_system_cows_rng_state(cpp11::sexp ptr);
extern "C" SEXP _cowflu_dust2_system_cows_rng_state(SEXP ptr) {
  BEGIN_CPP11
    return cpp11::as_sexp(dust2_system_cows_rng_state(cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(ptr)));
  END_CPP11
}
// cows.cpp
SEXP dust2_system_cows_set_rng_state(cpp11::sexp ptr, cpp11::sexp r_rng_state);
extern "C" SEXP _cowflu_dust2_system_cows_set_rng_state(SEXP ptr, SEXP r_rng_state) {
  BEGIN_CPP11
    return cpp11::as_sexp(dust2_system_cows_set_rng_state(cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(ptr), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_rng_state)));
  END_CPP11
}
// cows.cpp
SEXP dust2_system_cows_set_time(cpp11::sexp ptr, cpp11::sexp r_time);
extern "C" SEXP _cowflu_dust2_system_cows_set_time(SEXP ptr, SEXP r_time) {
  BEGIN_CPP11
    return cpp11::as_sexp(dust2_system_cows_set_time(cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(ptr), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_time)));
  END_CPP11
}
// cows.cpp
SEXP dust2_system_cows_update_pars(cpp11::sexp ptr, cpp11::list pars);
extern "C" SEXP _cowflu_dust2_system_cows_update_pars(SEXP ptr, SEXP pars) {
  BEGIN_CPP11
    return cpp11::as_sexp(dust2_system_cows_update_pars(cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(ptr), cpp11::as_cpp<cpp11::decay_t<cpp11::list>>(pars)));
  END_CPP11
}
// cows.cpp
SEXP dust2_system_cows_simulate(cpp11::sexp ptr, cpp11::sexp r_times, cpp11::sexp r_index_state, bool preserve_particle_dimension, bool preserve_group_dimension);
extern "C" SEXP _cowflu_dust2_system_cows_simulate(SEXP ptr, SEXP r_times, SEXP r_index_state, SEXP preserve_particle_dimension, SEXP preserve_group_dimension) {
  BEGIN_CPP11
    return cpp11::as_sexp(dust2_system_cows_simulate(cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(ptr), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_times), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_index_state), cpp11::as_cpp<cpp11::decay_t<bool>>(preserve_particle_dimension), cpp11::as_cpp<cpp11::decay_t<bool>>(preserve_group_dimension)));
  END_CPP11
}
// cows.cpp
SEXP dust2_unfilter_cows_alloc(cpp11::list r_pars, cpp11::sexp r_time_start, cpp11::sexp r_time, cpp11::list r_time_control, cpp11::list r_data, cpp11::sexp r_n_particles, cpp11::sexp r_n_groups, cpp11::sexp r_n_threads);
extern "C" SEXP _cowflu_dust2_unfilter_cows_alloc(SEXP r_pars, SEXP r_time_start, SEXP r_time, SEXP r_time_control, SEXP r_data, SEXP r_n_particles, SEXP r_n_groups, SEXP r_n_threads) {
  BEGIN_CPP11
    return cpp11::as_sexp(dust2_unfilter_cows_alloc(cpp11::as_cpp<cpp11::decay_t<cpp11::list>>(r_pars), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_time_start), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_time), cpp11::as_cpp<cpp11::decay_t<cpp11::list>>(r_time_control), cpp11::as_cpp<cpp11::decay_t<cpp11::list>>(r_data), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_n_particles), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_n_groups), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_n_threads)));
  END_CPP11
}
// cows.cpp
SEXP dust2_filter_cows_alloc(cpp11::list r_pars, cpp11::sexp r_time_start, cpp11::sexp r_time, cpp11::list r_time_control, cpp11::list r_data, cpp11::sexp r_n_particles, cpp11::sexp r_n_groups, cpp11::sexp r_n_threads, cpp11::sexp r_seed);
extern "C" SEXP _cowflu_dust2_filter_cows_alloc(SEXP r_pars, SEXP r_time_start, SEXP r_time, SEXP r_time_control, SEXP r_data, SEXP r_n_particles, SEXP r_n_groups, SEXP r_n_threads, SEXP r_seed) {
  BEGIN_CPP11
    return cpp11::as_sexp(dust2_filter_cows_alloc(cpp11::as_cpp<cpp11::decay_t<cpp11::list>>(r_pars), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_time_start), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_time), cpp11::as_cpp<cpp11::decay_t<cpp11::list>>(r_time_control), cpp11::as_cpp<cpp11::decay_t<cpp11::list>>(r_data), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_n_particles), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_n_groups), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_n_threads), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_seed)));
  END_CPP11
}
// cows.cpp
SEXP dust2_system_cows_compare_data(cpp11::sexp ptr, cpp11::list r_data, bool preserve_particle_dimension, bool preserve_group_dimension);
extern "C" SEXP _cowflu_dust2_system_cows_compare_data(SEXP ptr, SEXP r_data, SEXP preserve_particle_dimension, SEXP preserve_group_dimension) {
  BEGIN_CPP11
    return cpp11::as_sexp(dust2_system_cows_compare_data(cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(ptr), cpp11::as_cpp<cpp11::decay_t<cpp11::list>>(r_data), cpp11::as_cpp<cpp11::decay_t<bool>>(preserve_particle_dimension), cpp11::as_cpp<cpp11::decay_t<bool>>(preserve_group_dimension)));
  END_CPP11
}
// cows.cpp
SEXP dust2_unfilter_cows_update_pars(cpp11::sexp ptr, cpp11::list r_pars, cpp11::sexp r_index_group);
extern "C" SEXP _cowflu_dust2_unfilter_cows_update_pars(SEXP ptr, SEXP r_pars, SEXP r_index_group) {
  BEGIN_CPP11
    return cpp11::as_sexp(dust2_unfilter_cows_update_pars(cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(ptr), cpp11::as_cpp<cpp11::decay_t<cpp11::list>>(r_pars), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_index_group)));
  END_CPP11
}
// cows.cpp
SEXP dust2_unfilter_cows_run(cpp11::sexp ptr, cpp11::sexp r_initial, bool save_history, bool adjoint, cpp11::sexp r_index_state, cpp11::sexp r_index_group, bool preserve_particle_dimension, bool preserve_group_dimension);
extern "C" SEXP _cowflu_dust2_unfilter_cows_run(SEXP ptr, SEXP r_initial, SEXP save_history, SEXP adjoint, SEXP r_index_state, SEXP r_index_group, SEXP preserve_particle_dimension, SEXP preserve_group_dimension) {
  BEGIN_CPP11
    return cpp11::as_sexp(dust2_unfilter_cows_run(cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(ptr), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_initial), cpp11::as_cpp<cpp11::decay_t<bool>>(save_history), cpp11::as_cpp<cpp11::decay_t<bool>>(adjoint), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_index_state), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_index_group), cpp11::as_cpp<cpp11::decay_t<bool>>(preserve_particle_dimension), cpp11::as_cpp<cpp11::decay_t<bool>>(preserve_group_dimension)));
  END_CPP11
}
// cows.cpp
SEXP dust2_unfilter_cows_last_history(cpp11::sexp ptr, bool select_random_particle, bool preserve_particle_dimension, bool preserve_group_dimension);
extern "C" SEXP _cowflu_dust2_unfilter_cows_last_history(SEXP ptr, SEXP select_random_particle, SEXP preserve_particle_dimension, SEXP preserve_group_dimension) {
  BEGIN_CPP11
    return cpp11::as_sexp(dust2_unfilter_cows_last_history(cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(ptr), cpp11::as_cpp<cpp11::decay_t<bool>>(select_random_particle), cpp11::as_cpp<cpp11::decay_t<bool>>(preserve_particle_dimension), cpp11::as_cpp<cpp11::decay_t<bool>>(preserve_group_dimension)));
  END_CPP11
}
// cows.cpp
SEXP dust2_unfilter_cows_last_state(cpp11::sexp ptr, bool select_random_particle, bool preserve_particle_dimension, bool preserve_group_dimension);
extern "C" SEXP _cowflu_dust2_unfilter_cows_last_state(SEXP ptr, SEXP select_random_particle, SEXP preserve_particle_dimension, SEXP preserve_group_dimension) {
  BEGIN_CPP11
    return cpp11::as_sexp(dust2_unfilter_cows_last_state(cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(ptr), cpp11::as_cpp<cpp11::decay_t<bool>>(select_random_particle), cpp11::as_cpp<cpp11::decay_t<bool>>(preserve_particle_dimension), cpp11::as_cpp<cpp11::decay_t<bool>>(preserve_group_dimension)));
  END_CPP11
}
// cows.cpp
SEXP dust2_filter_cows_update_pars(cpp11::sexp ptr, cpp11::list r_pars, cpp11::sexp r_index_group);
extern "C" SEXP _cowflu_dust2_filter_cows_update_pars(SEXP ptr, SEXP r_pars, SEXP r_index_group) {
  BEGIN_CPP11
    return cpp11::as_sexp(dust2_filter_cows_update_pars(cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(ptr), cpp11::as_cpp<cpp11::decay_t<cpp11::list>>(r_pars), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_index_group)));
  END_CPP11
}
// cows.cpp
SEXP dust2_filter_cows_run(cpp11::sexp ptr, cpp11::sexp r_initial, bool save_history, bool adjoint, cpp11::sexp index_state, cpp11::sexp index_group, bool preserve_particle_dimension, bool preserve_group_dimension);
extern "C" SEXP _cowflu_dust2_filter_cows_run(SEXP ptr, SEXP r_initial, SEXP save_history, SEXP adjoint, SEXP index_state, SEXP index_group, SEXP preserve_particle_dimension, SEXP preserve_group_dimension) {
  BEGIN_CPP11
    return cpp11::as_sexp(dust2_filter_cows_run(cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(ptr), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_initial), cpp11::as_cpp<cpp11::decay_t<bool>>(save_history), cpp11::as_cpp<cpp11::decay_t<bool>>(adjoint), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(index_state), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(index_group), cpp11::as_cpp<cpp11::decay_t<bool>>(preserve_particle_dimension), cpp11::as_cpp<cpp11::decay_t<bool>>(preserve_group_dimension)));
  END_CPP11
}
// cows.cpp
SEXP dust2_filter_cows_last_history(cpp11::sexp ptr, bool select_random_particle, bool preserve_particle_dimension, bool preserve_group_dimension);
extern "C" SEXP _cowflu_dust2_filter_cows_last_history(SEXP ptr, SEXP select_random_particle, SEXP preserve_particle_dimension, SEXP preserve_group_dimension) {
  BEGIN_CPP11
    return cpp11::as_sexp(dust2_filter_cows_last_history(cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(ptr), cpp11::as_cpp<cpp11::decay_t<bool>>(select_random_particle), cpp11::as_cpp<cpp11::decay_t<bool>>(preserve_particle_dimension), cpp11::as_cpp<cpp11::decay_t<bool>>(preserve_group_dimension)));
  END_CPP11
}
// cows.cpp
SEXP dust2_filter_cows_last_state(cpp11::sexp ptr, bool select_random_particle, bool preserve_particle_dimension, bool preserve_group_dimension);
extern "C" SEXP _cowflu_dust2_filter_cows_last_state(SEXP ptr, SEXP select_random_particle, SEXP preserve_particle_dimension, SEXP preserve_group_dimension) {
  BEGIN_CPP11
    return cpp11::as_sexp(dust2_filter_cows_last_state(cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(ptr), cpp11::as_cpp<cpp11::decay_t<bool>>(select_random_particle), cpp11::as_cpp<cpp11::decay_t<bool>>(preserve_particle_dimension), cpp11::as_cpp<cpp11::decay_t<bool>>(preserve_group_dimension)));
  END_CPP11
}
// cows.cpp
SEXP dust2_filter_cows_rng_state(cpp11::sexp ptr);
extern "C" SEXP _cowflu_dust2_filter_cows_rng_state(SEXP ptr) {
  BEGIN_CPP11
    return cpp11::as_sexp(dust2_filter_cows_rng_state(cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(ptr)));
  END_CPP11
}
// cows.cpp
SEXP dust2_filter_cows_set_rng_state(cpp11::sexp ptr, cpp11::sexp r_rng_state);
extern "C" SEXP _cowflu_dust2_filter_cows_set_rng_state(SEXP ptr, SEXP r_rng_state) {
  BEGIN_CPP11
    return cpp11::as_sexp(dust2_filter_cows_set_rng_state(cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(ptr), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_rng_state)));
  END_CPP11
}

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"_cowflu_dust2_filter_cows_alloc",             (DL_FUNC) &_cowflu_dust2_filter_cows_alloc,             9},
    {"_cowflu_dust2_filter_cows_last_history",      (DL_FUNC) &_cowflu_dust2_filter_cows_last_history,      4},
    {"_cowflu_dust2_filter_cows_last_state",        (DL_FUNC) &_cowflu_dust2_filter_cows_last_state,        4},
    {"_cowflu_dust2_filter_cows_rng_state",         (DL_FUNC) &_cowflu_dust2_filter_cows_rng_state,         1},
    {"_cowflu_dust2_filter_cows_run",               (DL_FUNC) &_cowflu_dust2_filter_cows_run,               8},
    {"_cowflu_dust2_filter_cows_set_rng_state",     (DL_FUNC) &_cowflu_dust2_filter_cows_set_rng_state,     2},
    {"_cowflu_dust2_filter_cows_update_pars",       (DL_FUNC) &_cowflu_dust2_filter_cows_update_pars,       3},
    {"_cowflu_dust2_system_cows_alloc",             (DL_FUNC) &_cowflu_dust2_system_cows_alloc,             8},
    {"_cowflu_dust2_system_cows_compare_data",      (DL_FUNC) &_cowflu_dust2_system_cows_compare_data,      4},
    {"_cowflu_dust2_system_cows_reorder",           (DL_FUNC) &_cowflu_dust2_system_cows_reorder,           2},
    {"_cowflu_dust2_system_cows_rng_state",         (DL_FUNC) &_cowflu_dust2_system_cows_rng_state,         1},
    {"_cowflu_dust2_system_cows_run_to_time",       (DL_FUNC) &_cowflu_dust2_system_cows_run_to_time,       2},
    {"_cowflu_dust2_system_cows_set_rng_state",     (DL_FUNC) &_cowflu_dust2_system_cows_set_rng_state,     2},
    {"_cowflu_dust2_system_cows_set_state",         (DL_FUNC) &_cowflu_dust2_system_cows_set_state,         2},
    {"_cowflu_dust2_system_cows_set_state_initial", (DL_FUNC) &_cowflu_dust2_system_cows_set_state_initial, 1},
    {"_cowflu_dust2_system_cows_set_time",          (DL_FUNC) &_cowflu_dust2_system_cows_set_time,          2},
    {"_cowflu_dust2_system_cows_simulate",          (DL_FUNC) &_cowflu_dust2_system_cows_simulate,          5},
    {"_cowflu_dust2_system_cows_state",             (DL_FUNC) &_cowflu_dust2_system_cows_state,             6},
    {"_cowflu_dust2_system_cows_time",              (DL_FUNC) &_cowflu_dust2_system_cows_time,              1},
    {"_cowflu_dust2_system_cows_update_pars",       (DL_FUNC) &_cowflu_dust2_system_cows_update_pars,       2},
    {"_cowflu_dust2_unfilter_cows_alloc",           (DL_FUNC) &_cowflu_dust2_unfilter_cows_alloc,           8},
    {"_cowflu_dust2_unfilter_cows_last_history",    (DL_FUNC) &_cowflu_dust2_unfilter_cows_last_history,    4},
    {"_cowflu_dust2_unfilter_cows_last_state",      (DL_FUNC) &_cowflu_dust2_unfilter_cows_last_state,      4},
    {"_cowflu_dust2_unfilter_cows_run",             (DL_FUNC) &_cowflu_dust2_unfilter_cows_run,             8},
    {"_cowflu_dust2_unfilter_cows_update_pars",     (DL_FUNC) &_cowflu_dust2_unfilter_cows_update_pars,     3},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_cowflu(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
