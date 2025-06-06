#include <dust2/common.hpp>
#include <numeric>

enum likelihood_type { INCIDENCE, SURVIVAL };

likelihood_type read_likelihood_type(cpp11::list pars, const char * name) {
  cpp11::sexp r_likelihood_choice = pars[name];
  if (r_likelihood_choice == R_NilValue) {
    return INCIDENCE;
  }
  if (TYPEOF(r_likelihood_choice) != STRSXP || LENGTH(r_likelihood_choice) != 1) {
    cpp11::stop("Expected '%s' to be a string", name);
  }
  std::string likelihood_choice = cpp11::as_cpp<std::string>(r_likelihood_choice);
  if (likelihood_choice == "incidence") {
    return INCIDENCE;
  } else if (likelihood_choice == "survival") {
    return SURVIVAL;
  } else {
    cpp11::stop("Invalid value for '%s': '%s'",
                name, likelihood_choice.c_str());
  }
}

template <typename real_type>
real_type nudge(bool x, real_type eps) {
  return x == 0 ? eps : 1 - eps;
}

template <typename real_type>
void sum_over_regions(real_type *cows,
                      const size_t n_herds,
                      const size_t n_regions,
                      const std::vector<size_t>& region_start) {
  real_type * dest = cows + n_herds;
  for (size_t i = 0; i < n_regions; ++i) {
    const size_t i_start = region_start[i];
    const size_t i_end = region_start[i + 1];
    dest[i] = std::accumulate(cows + i_start, cows + i_end, 0);
  }
}

struct outbreak_detection_parameters {
  bool proportion_only;
  double N_scaling;
  double proportion_scaling;
  double strength_scaling;
  double I_scaling;
};


template <typename real_type, typename rng_state_type>
bool declare_outbreak_in_herd(real_type I, real_type N, real_type asc_rate, const outbreak_detection_parameters& pars, real_type dt, rng_state_type& rng_state) {
  const auto u = monty::random::random_real<double>(rng_state);
  if (pars.proportion_only) {
    const auto scaling = pars.proportion_scaling;
    const auto prevalence = I / N * asc_rate * dt;
    const auto logistic_prevalence = prevalence / std::pow((1 + std::pow(prevalence, scaling)), 1 / scaling);
    return u < logistic_prevalence;
  } else {
    const auto N_scaling = pars.N_scaling;
    const auto strength_scaling = pars.strength_scaling;
    const auto I_scaling = pars.I_scaling;
    const auto prevalence = (I / std::pow(N_scaling * N , strength_scaling) + I / I_scaling) * asc_rate * dt;
    const auto bounded_prevalence = 1 - std::exp(-prevalence);
    const auto u = monty::random::random_real<double>(rng_state);
    return u < bounded_prevalence;
  }
}

// [[dust2::class(cows)]]
// [[dust2::time_type(discrete)]]
// [[dust2::has_compare()]]
class cows {
public:
  cows() = delete;

  using real_type = double;

  struct shared_state {
    size_t n_seed;
    std::vector<size_t> seed_time;
    std::vector<size_t> seed_herd;
    std::vector<size_t> seed_amount;

    size_t n_herds;
    size_t n_regions;
    real_type gamma;
    real_type sigma;
    real_type mu;
    real_type beta;
    real_type alpha;
    real_type time_test;
    real_type n_test;
    likelihood_type likelihood_choice;
    std::vector<size_t> region_start;
    std::vector<size_t> herd_to_region_lookup;
    std::vector<real_type> p_region_export;
    std::vector<real_type> p_cow_export;
    std::vector<real_type> n_cows_per_herd;
    std::vector<real_type> movement_matrix;
    real_type start_count;
    size_t start_herd;
    std::vector<real_type> asc_rate;
    real_type dispersion;
    bool condition_on_export;
    bool export_prob_depends_on_size;
    // A bunch of control for the outbreak detection, except for
    // asc_rate which is something we want to fit to, and which varies
    // by region (everything here holds for the whole simulation).
    // This exists so that we can pass it all through to the
    // declare_outbreak_in_herd function neatly.
    outbreak_detection_parameters outbreak_detection;
  };

  struct internal_state {
    // population size per herd
    std::vector<real_type> N;
    std::vector<real_type> export_S;
    std::vector<real_type> export_E;
    std::vector<real_type> export_I;
    std::vector<real_type> export_R;
    std::vector<real_type> import_S;
    std::vector<real_type> import_E;
    std::vector<real_type> import_I;
    std::vector<real_type> import_R;
    bool has_exported;
  };

  using rng_state_type = monty::random::generator<real_type>;

  static auto packing_state(const shared_state& shared) {
    return dust2::packing{{"S_herd", {shared.n_herds}}, {"S_region", {shared.n_regions}}, {"E_herd", {shared.n_herds}}, {"E_region", {shared.n_regions}}, {"I_herd", {shared.n_herds}}, {"I_region", {shared.n_regions}}, {"R_herd", {shared.n_herds}}, {"R_region", {shared.n_regions}}, {"outbreak_herd", {shared.n_herds}}, {"outbreak_region", {shared.n_regions}}, {"infected_herds_region", {shared.n_regions}}, {"probability_test_pass_region", {shared.n_regions}}};
  }

  static auto packing_gradient(const shared_state& shared) {
    return dust2::packing{};
  }

  static void initial(real_type time,
                      const shared_state& shared,
                      internal_state& internal,
                      rng_state_type& rng_state,
                      real_type * state_next) {
    // Start by zeroing everything
    const auto len_state = 5 * (shared.n_herds + shared.n_regions) + 2 * shared.n_regions;
    std::fill(state_next, state_next + len_state, 0);
    // Then fill in susceptibles from the mean herd size
    const size_t n = shared.n_herds + shared.n_regions;
    auto *S = state_next;
    auto *E = state_next + 1 * n;
    auto *I = state_next + 2 * n;
    auto *R = state_next + 3 * n;
    auto *infected_herds_region = state_next + 5 * n;
    auto *I_region = state_next + 2 * n + shared.n_herds;
    auto *probability_test_pass_region = state_next + 5 * n + shared.n_regions;

    std::copy(shared.n_cows_per_herd.begin(), shared.n_cows_per_herd.end(), S);
    // Seed the infections into the I class
    I[shared.start_herd] = shared.start_count;
    S[shared.start_herd] -= shared.start_count;

    sum_over_regions(S, shared.n_herds, shared.n_regions, shared.region_start);
    sum_over_regions(I, shared.n_herds, shared.n_regions, shared.region_start);

    std::transform(I_region, I_region + shared.n_regions, infected_herds_region,
              [](int x) { return (x > 0) ? 1 : 0; });

    // Initialise probabilities of passing export tests:
    for (size_t i = 0; i < shared.n_regions; ++i) {
      const size_t i_start = shared.region_start[i];
      const size_t i_end = shared.region_start[i + 1];
      double pass_probability_tally = 0.0;
      for (size_t j = i_start; j < i_end; ++j) {
        pass_probability_tally += monty::density::hypergeometric(0.0, std::round(I[j]*shared.p_cow_export[i]), std::round((S[j] + E[j] + R[j])*shared.p_cow_export[i]), std::min(static_cast<real_type>(shared.n_test), std::round((S[j] + E[j] + R[j])*shared.p_cow_export[i]) + std::round(I[j]*shared.p_cow_export[i]) ), false )  / (i_end - i_start);
      }
      probability_test_pass_region[i] = pass_probability_tally;
    }

  }

  // The main update function, converting state to state_next
  static void update(real_type time,
                     real_type dt,
                     const real_type * state,
                     const shared_state& shared,
                     internal_state& internal,
                     rng_state_type& rng_state,
                     real_type * state_next) {
    const size_t n = shared.n_herds + shared.n_regions;
    const real_type* S = state;
    const real_type* E = state + n;
    const real_type* I = state + 2 * n;
    const real_type* R = state + 3 * n;
    const real_type* outbreak = state + 4 * n;
    const real_type* outbreak_region_count = outbreak + shared.n_herds;
    const real_type* infected_herds_region = outbreak_region_count + shared.n_regions;
    const real_type* probability_test_pass_region = infected_herds_region + shared.n_regions;

    real_type* S_next = state_next;
    real_type* E_next = state_next + n;
    real_type* I_next = state_next + 2 * n;
    real_type* R_next = state_next + 3 * n;
    real_type* outbreak_next = state_next + 4 * n;
    real_type* outbreak_region_count_next = outbreak_next + shared.n_herds;
    real_type* infected_herds_region_next = outbreak_region_count_next + shared.n_regions;
    real_type* probability_test_pass_region_next = infected_herds_region_next + shared.n_regions;

    for (size_t i = 0; i < shared.n_herds; ++i) {
      internal.N[i] = S[i] + E[i] + I[i] + R[i];
    }
    const real_type p_EI = 1 - std::exp(-shared.sigma * dt); // E to I
    const real_type p_IR = 1 - std::exp(-shared.gamma * dt); // I to R

    for (size_t i = 0; i < shared.n_regions; ++i) {
      const size_t i_start = shared.region_start[i];
      const size_t i_end = shared.region_start[i + 1];

      const real_type tot_I = std::accumulate(I + i_start, I + i_end, 0);
      const real_type tot_N = std::accumulate(internal.N.begin() + i_start, internal.N.begin() + i_end, 0);
      size_t n_outbreaks = 0;
      for (size_t j = i_start; j < i_end; ++j) {
        const real_type lambda = (internal.N[j] == 0 || (tot_N - internal.N[j]) == 0) ? 0 :
          ( (shared.beta*shared.gamma) * (I[j] / internal.N[j] + shared.alpha * (tot_I - I[j]) / (tot_N - internal.N[j])));
        const real_type p_SE = 1 - std::exp(-lambda * dt); // S to E
        const real_type n_SE = monty::random::binomial<real_type>(rng_state, S[j], p_SE);
        const real_type n_EI = monty::random::binomial<real_type>(rng_state, E[j], p_EI);
        const real_type n_IR = monty::random::binomial<real_type>(rng_state, I[j], p_IR);
        // Calculate births and (natural) deaths
        const real_type n_births = monty::random::binomial<real_type>(rng_state, S[j]+E[j]+I[j]+R[j], 1 - std::exp(-shared.mu * dt));
        const real_type n_deaths_S = monty::random::binomial<real_type>(rng_state, S[j] - n_SE, 1 - std::exp(-shared.mu * dt));
        const real_type n_deaths_E = monty::random::binomial<real_type>(rng_state, E[j] + n_SE - n_EI, 1 - std::exp(-shared.mu * dt));
        const real_type n_deaths_I = monty::random::binomial<real_type>(rng_state, I[j] + n_EI - n_IR, 1 - std::exp(-shared.mu * dt));
        const real_type n_deaths_R = monty::random::binomial<real_type>(rng_state, R[j] + n_IR, 1 - std::exp(-shared.mu * dt));

        S_next[j] = S[j] - n_SE  + n_births - n_deaths_S;
        E_next[j] = E[j] + n_SE - n_EI  - n_deaths_E;
        I_next[j] = I[j] + n_EI - n_IR  - n_deaths_I;
        R_next[j] = R[j] + n_IR  - n_deaths_R;

        // Update internal N to account for births/deaths
        internal.N[j] = S_next[j] + E_next[j] + I_next[j] + R_next[j];

        // Check if we have declared an outbreak in this herd, add
        // that to the region total if so.
        if (outbreak[j]) {
          outbreak_next[j] = true;
        } else {
          const auto new_outbreak = declare_outbreak_in_herd(I_next[j], internal.N[j], shared.asc_rate[i], shared.outbreak_detection, dt, rng_state);
          outbreak_next[j] = new_outbreak;
          if (new_outbreak) {
            n_outbreaks++;
          }
        }
      }
      outbreak_region_count_next[i] = outbreak_region_count[i] + n_outbreaks;
    }

    std::fill(internal.export_S.begin(), internal.export_S.end(), 0);
    std::fill(internal.export_E.begin(), internal.export_E.end(), 0);
    std::fill(internal.export_I.begin(), internal.export_I.end(), 0);
    std::fill(internal.export_R.begin(), internal.export_R.end(), 0);
    std::fill(internal.import_S.begin(), internal.import_S.end(), 0);
    std::fill(internal.import_E.begin(), internal.import_E.end(), 0);
    std::fill(internal.import_I.begin(), internal.import_I.end(), 0);
    std::fill(internal.import_R.begin(), internal.import_R.end(), 0);

    // Above, we change the populations (we do this BEFORE calculating import/exports)

    // Calculate exports
    if(shared.export_prob_depends_on_size){
      // Bigger herds are more likely to export

      // Convert N into cumulative counts within a region:
      for (size_t i = 0; i < shared.n_regions; ++i) {
        const size_t i_start = shared.region_start[i];
        const size_t i_end = shared.region_start[i + 1];
        for (size_t j = i_start + 1; j < i_end; ++j) {
          internal.N[j] += internal.N[j - 1];
        }
      }

      // Calculate how many exports we will have from each region
      for (size_t j = 0; j < shared.n_regions; ++j) {
        const auto p_cow_export = shared.p_cow_export[j];
        const auto logistic_p_region = (shared.p_region_export[j] * dt) / std::pow((1 + std::pow((shared.p_region_export[j] * dt), 10)), 0.1);
        const auto j_region_start = shared.region_start[j];
        const auto j_region_end = shared.region_start[j + 1];
        const size_t n_herds_in_region = j_region_end - j_region_start;
        const auto region_exports = monty::random::binomial<real_type>(rng_state, n_herds_in_region, logistic_p_region);
        // Assign these exports to specific herds dependent on their size.
        // Can't assign multiple exports to the same herd.

        //Assign all exports
        for (size_t k = 0; k < region_exports; ++k) {
          // A switch to say if the export was successfully assigned.
          bool export_assigned = false;
          do {
            const real_type u2 = monty::random::random_real<real_type>(rng_state);
            const size_t n_cows_in_region = internal.N[j_region_end - 1];
            const auto it_N = internal.N.begin() + j_region_start;
            const size_t export_dst = j_region_start + std::distance(it_N, std::upper_bound(it_N, it_N + n_herds_in_region, u2 * n_cows_in_region));

            internal.has_exported = internal.export_S[export_dst] > 0 || internal.export_E[export_dst] > 0 || internal.export_I[export_dst] > 0 || internal.export_R[export_dst] > 0;
            if(!internal.has_exported){
              internal.export_S[export_dst] = monty::random::binomial<real_type>(rng_state, S_next[export_dst], p_cow_export);
              internal.export_E[export_dst] = monty::random::binomial<real_type>(rng_state, E_next[export_dst], p_cow_export);
              internal.export_I[export_dst] = monty::random::binomial<real_type>(rng_state, I_next[export_dst], p_cow_export);
              internal.export_R[export_dst] = monty::random::binomial<real_type>(rng_state, R_next[export_dst], p_cow_export);
              export_assigned = true;
            }

          } while (!export_assigned);

        }

      }
    } else {
      // All herds have equal chance of exporting
      for (size_t i = 0; i < shared.n_herds; ++i) {
        const auto j = shared.herd_to_region_lookup[i];
        // TODO: thom to investigate
        //
        // region export through logistic function, or possibly as 1 -
        // exp(dt * p_region_export), but multiplication by dt means
        // that we overestimate this export at large dt.
        const auto logistic_p_region = (shared.p_region_export[j] * dt) / std::pow((1 + std::pow((shared.p_region_export[j] * dt), 10)), 0.1);
        const auto export_cows = internal.N[i] > 0 && monty::random::random_real<real_type>(rng_state) < logistic_p_region;
        if (export_cows) {
          const auto p_cow_export = shared.p_cow_export[j];
          // Option 1: rejection sampling:
          size_t n_exported = 0;
          do {
            internal.export_S[i] = monty::random::binomial<real_type>(rng_state, S_next[i], p_cow_export);
            internal.export_E[i] = monty::random::binomial<real_type>(rng_state, E_next[i], p_cow_export);
            internal.export_I[i] = monty::random::binomial<real_type>(rng_state, I_next[i], p_cow_export);
            internal.export_R[i] = monty::random::binomial<real_type>(rng_state, R_next[i], p_cow_export);
            n_exported = internal.export_S[i] + internal.export_E[i] + internal.export_I[i] + internal.export_R[i];
          } while (shared.condition_on_export && n_exported == 0);
          // Option 2:
          //
          // Sample the number of cows in each compartment from a
          // beta-binomial, sharing the beta draw across the four draws,
          // but redrawing each time around the rejection.
          //
          // Option 3:
          //
          // If p is very small, then sample from a conditioned binomial
          // for the total over all cows, then draw SEIR allocation from
          // a multivartiate hypergeometric, which is not actually
          // implemented in monty yet.
        }
      }


      // Convert N into cumulative counts within a region:
      for (size_t i = 0; i < shared.n_regions; ++i) {
        const size_t i_start = shared.region_start[i];
        const size_t i_end = shared.region_start[i + 1];
        for (size_t j = i_start + 1; j < i_end; ++j) {
          internal.N[j] += internal.N[j - 1];
        }
      }
    }

    const auto state_travel_allowed = time < shared.time_test;
    for (size_t i_src = 0; i_src < shared.n_herds; ++i_src) {
      const size_t export_N = internal.export_S[i_src] + internal.export_E[i_src] + internal.export_I[i_src] + internal.export_R[i_src];
      if (export_N > 0) {
        const size_t i_region_src = shared.herd_to_region_lookup[i_src];
        const auto p = shared.movement_matrix.begin() + i_region_src * shared.n_regions;
        const real_type u1 = monty::random::random_real<real_type>(rng_state);
        const size_t i_region_dst = std::distance(p, std::upper_bound(p, p + shared.n_regions, u1));
        const auto within_region = i_region_src == i_region_dst;
        const real_type u2 = monty::random::random_real<real_type>(rng_state);

        const auto i_region_start = shared.region_start[i_region_dst];
        const auto i_region_end = shared.region_start[i_region_dst + 1];

        const size_t n_herds_in_region = i_region_end - i_region_start;
        const size_t n_cows_in_region = internal.N[i_region_end - 1];
        const auto it_N = internal.N.begin() + i_region_start;
        const size_t i_dst = i_region_start + std::distance(it_N, std::upper_bound(it_N, it_N + n_herds_in_region, u2 * n_cows_in_region));

        //TODO: Look at how long herds are barred from exporting for
        const bool test_herds = within_region || state_travel_allowed ||
          monty::random::hypergeometric(rng_state, internal.export_I[i_src], export_N - internal.export_I[i_src], std::min(shared.n_test, static_cast<real_type>(export_N))) == 0;
        const bool allow_movement = test_herds && ! outbreak[i_src];
        if (allow_movement) {
          internal.import_S[i_dst] += internal.export_S[i_src];
          internal.import_E[i_dst] += internal.export_E[i_src];
          internal.import_I[i_dst] += internal.export_I[i_src];
          internal.import_R[i_dst] += internal.export_R[i_src];
        } else {
          internal.export_S[i_src] = 0;
          internal.export_E[i_src] = 0;
          internal.export_I[i_src] = 0;
          internal.export_R[i_src] = 0;
        }
      }
    }

    for (size_t i = 0; i < shared.n_herds; ++i) {
      S_next[i] = S_next[i] + internal.import_S[i] - internal.export_S[i];
      E_next[i] = E_next[i] + internal.import_E[i] - internal.export_E[i];
      I_next[i] = I_next[i] + internal.import_I[i] - internal.export_I[i];
      R_next[i] = R_next[i] + internal.import_R[i] - internal.export_R[i];
    }

    // Add custom seeding
    for (size_t i = 0; i < shared.n_seed; ++i) {
      if(time == shared.seed_time[i]){
        size_t cows_to_seed = std::min(shared.seed_amount[i], static_cast<size_t>(S_next[shared.seed_herd[i]-1]));
        S_next[shared.seed_herd[i]-1] = S_next[shared.seed_herd[i]-1] - cows_to_seed;
        I_next[shared.seed_herd[i]-1] = I_next[shared.seed_herd[i]-1] + cows_to_seed;
      }
    }

    sum_over_regions(S_next, shared.n_herds, shared.n_regions, shared.region_start);
    sum_over_regions(E_next, shared.n_herds, shared.n_regions, shared.region_start);
    sum_over_regions(I_next, shared.n_herds, shared.n_regions, shared.region_start);
    sum_over_regions(R_next, shared.n_herds, shared.n_regions, shared.region_start);

    //Tally how many herd in each region contain ANY infected cows:
    for (size_t i = 0; i < shared.n_regions; ++i) {
      const size_t i_start = shared.region_start[i];
      const size_t i_end = shared.region_start[i + 1];
      size_t infected_herds_tally = 0;
      for (size_t j = i_start; j < i_end; ++j) {
        infected_herds_tally += (I_next[j] > 0);
      }
      infected_herds_region_next[i] = infected_herds_tally;
    }

    // Record the probability a randomly selected herd passing the border export test per region
    for (size_t i = 0; i < shared.n_regions; ++i) {
      const size_t i_start = shared.region_start[i];
      const size_t i_end = shared.region_start[i + 1];
      double pass_probability_tally = 0.0;
      for (size_t j = i_start; j < i_end; ++j) {
        pass_probability_tally += monty::density::hypergeometric(0.0, std::round(I_next[j]*shared.p_cow_export[i]), std::round((S_next[j] + E_next[j] + R_next[j])*shared.p_cow_export[i]), std::min(static_cast<real_type>(shared.n_test), std::round((S_next[j] + E_next[j] + R_next[j])*shared.p_cow_export[i]) + std::round(I_next[j])*shared.p_cow_export[i] ), false )  / (i_end - i_start);
      }
      probability_test_pass_region_next[i] = pass_probability_tally;
    }

  }

  static shared_state build_shared(cpp11::list pars) {
    const size_t n_herds = dust2::r::read_size(pars, "n_herds");
    const size_t n_regions = dust2::r::read_size(pars, "n_regions");
    std::vector<size_t> region_start(n_regions + 1);
    auto r_region_start = pars["region_start"];
    const int * region_start_data = INTEGER(r_region_start);
    for (size_t i = 0; i < n_regions; ++i) {
      region_start[i] = region_start_data[i];
    }
    region_start[n_regions] = n_herds;

    std::vector<size_t> herd_to_region_lookup;
    herd_to_region_lookup.reserve(n_herds);
    for (size_t i = 0; i < n_regions; ++i) {
      for (size_t j = region_start[i]; j < region_start[i + 1]; ++j) {
        herd_to_region_lookup.push_back(i);
      }
    }

    if (herd_to_region_lookup.size() != n_herds) {
      cpp11::stop("Error while building lookup");
    }

    std::vector<real_type> p_region_export(n_regions);
    std::vector<real_type> p_cow_export(n_regions);
    std::vector<real_type> n_cows_per_herd(n_herds);
    dust2::r::read_real_vector(pars, n_regions, p_region_export.data(), "p_region_export", true);
    dust2::r::read_real_vector(pars, n_regions, p_cow_export.data(), "p_cow_export", true);
    dust2::r::read_real_vector(pars, n_herds, n_cows_per_herd.data(), "n_cows_per_herd", true);

    std::vector<real_type> movement_matrix(n_regions * n_regions);
    dust2::r::read_real_vector(pars, n_regions * n_regions, movement_matrix.data(), "movement_matrix", true);

    const bool condition_on_export = dust2::r::read_bool(pars, "condition_on_export", true);
    const bool export_prob_depends_on_size = dust2::r::read_bool(pars, "export_prob_depends_on_size", false);

    const real_type time_test = dust2::r::read_real(pars, "time_test", 30);
    const real_type n_test = dust2::r::read_real(pars, "n_test", 30);

    const real_type start_count = dust2::r::read_real(pars, "start_count");
    const size_t start_herd = dust2::r::read_size(pars, "start_herd") - 1;

    const real_type beta = dust2::r::read_real(pars, "beta");
    const real_type gamma = dust2::r::read_real(pars, "gamma");
    const real_type alpha = dust2::r::read_real(pars, "alpha");
    const real_type sigma = dust2::r::read_real(pars, "sigma");
    const real_type mu = dust2::r::read_real(pars, "mu", 0);
    const real_type dispersion = dust2::r::read_real(pars, "dispersion");

    cpp11::sexp r_asc_rate = pars["asc_rate"];
    std::vector<real_type> asc_rate(n_regions);
    if (LENGTH(r_asc_rate) == 1) {
      std::fill(asc_rate.begin(),
                asc_rate.end(),
                dust2::r::read_real(pars, "asc_rate"));
    } else {
      dust2::r::read_real_vector(pars, n_regions, asc_rate.data(), "asc_rate", true);
    }

    const auto likelihood_choice = read_likelihood_type(pars, "likelihood_choice");

    const bool outbreak_detection_proportion_only = dust2::r::read_bool(pars, "outbreak_detection_proportion_only", false);
    const real_type N_scaling = dust2::r::read_real(pars, "N_scaling", 0.7);
    const real_type proportion_scaling = dust2::r::read_real(pars, "proportion_scaling", 10);
    const real_type strength_scaling = dust2::r::read_real(pars, "strength_scaling", 0.95);
    const real_type I_scaling = dust2::r::read_real(pars, "I_scaling", 150);

    const outbreak_detection_parameters outbreak_detection{outbreak_detection_proportion_only, N_scaling, proportion_scaling, strength_scaling, I_scaling};

    const size_t n_seed = dust2::r::read_size(pars, "n_seed");
    std::vector<size_t> seed_time(n_seed);
    std::vector<size_t> seed_herd(n_seed);
    std::vector<size_t> seed_amount(n_seed);

    auto r_seed_time = pars["seed_time"];
    auto r_seed_herd = pars["seed_herd"];
    auto r_seed_amount = pars["seed_amount"];
    const int * seed_time_data = INTEGER(r_seed_time);
    const int * seed_herd_data = INTEGER(r_seed_herd);
    const int * seed_amount_data = INTEGER(r_seed_amount);
    for (size_t i = 0; i < n_seed; ++i) {
      seed_time[i] = seed_time_data[i];
      seed_herd[i] = seed_herd_data[i];
      seed_amount[i] = seed_amount_data[i];
    }
    // dust2::r::read_real_vector(pars, n_seed, seed_time.data(), "seed_time", true);
    // dust2::r::read_real_vector(pars, n_seed, seed_herd.data(), "seed_herd", true);
    // dust2::r::read_real_vector(pars, n_seed, seed_amount.data(), "seed_amount", true);

    return shared_state{n_seed, seed_time, seed_herd, seed_amount, n_herds, n_regions, gamma, sigma, mu, beta, alpha, time_test, n_test, likelihood_choice, region_start, herd_to_region_lookup, p_region_export, p_cow_export, n_cows_per_herd, movement_matrix, start_count, start_herd, asc_rate, dispersion, condition_on_export, export_prob_depends_on_size, outbreak_detection};
  }

  static internal_state build_internal(const shared_state& shared) {
    std::vector<real_type> N(shared.n_herds);
    std::vector<real_type> export_S(shared.n_herds);
    std::vector<real_type> export_E(shared.n_herds);
    std::vector<real_type> export_I(shared.n_herds);
    std::vector<real_type> export_R(shared.n_herds);
    std::vector<real_type> import_S(shared.n_herds);
    std::vector<real_type> import_E(shared.n_herds);
    std::vector<real_type> import_I(shared.n_herds);
    std::vector<real_type> import_R(shared.n_herds);
    bool has_exported = false;
    return internal_state{N, export_S, export_E, export_I, export_R, import_S, import_E, import_I, import_R, has_exported};
  }

  // This is the bit that we'll use to do fast parameter updating, and
  // we'll guarantee somewhere that the size does not change.
  static void update_shared(cpp11::list pars, shared_state& shared) {
    shared.beta = dust2::r::read_real(pars, "beta", shared.beta);
    shared.gamma = dust2::r::read_real(pars, "gamma", shared.gamma);
    shared.alpha = dust2::r::read_real(pars, "alpha", shared.alpha);
    shared.sigma = dust2::r::read_real(pars, "sigma", shared.sigma);
    shared.mu = dust2::r::read_real(pars, "mu", shared.mu);
    shared.dispersion = dust2::r::read_real(pars, "dispersion", shared.dispersion);

    if (LENGTH(pars["asc_rate"]) == 1) {
      std::fill(shared.asc_rate.begin(),
                shared.asc_rate.end(),
                dust2::r::read_real(pars, "asc_rate"));
    } else {
      dust2::r::read_real_vector(pars, shared.n_regions, shared.asc_rate.data(), "asc_rate", false);
    }

    shared.outbreak_detection.N_scaling = dust2::r::read_real(pars, "N_scaling", shared.outbreak_detection.N_scaling);
    shared.outbreak_detection.proportion_scaling = dust2::r::read_real(pars, "proportion_scaling", shared.outbreak_detection.proportion_scaling);
    shared.outbreak_detection.strength_scaling = dust2::r::read_real(pars, "strength_scaling", shared.outbreak_detection.strength_scaling);
    shared.outbreak_detection.I_scaling = dust2::r::read_real(pars, "I_scaling", shared.outbreak_detection.I_scaling);

  }

  // This is a reasonable default implementation in the no-internal
  // case
  static void update_internal(const shared_state& shared,
                              internal_state& internal) {
  }

  static auto zero_every(const shared_state& shared) {
    std::vector<size_t> reset;
    const auto offset = 4 * (shared.n_herds + shared.n_regions) + shared.n_herds;
    for (size_t i = 0; i < shared.n_regions; ++i) {
      reset.push_back(i + offset);
    }
    return dust2::zero_every_type<real_type>{{1, reset}};
  }

  struct data_type {
    std::vector<real_type> positive_tests;
    std::vector<real_type> outbreak_detected;
  };

  static data_type build_data(cpp11::list r_data, const shared_state& shared) {
    auto data = static_cast<cpp11::list>(r_data);
    const auto n_regions = shared.n_regions;
    std::vector<real_type> positive_tests;
    std::vector<real_type> outbreak_detected;
    if (shared.likelihood_choice == INCIDENCE) {
      positive_tests.resize(n_regions);
      dust2::r::read_real_vector(r_data, shared.n_regions, positive_tests.data(), "positive_tests", true);
    } else {
      outbreak_detected.resize(n_regions);
      dust2::r::read_real_vector(r_data, shared.n_regions, outbreak_detected.data(), "outbreak_detected", true);
    }
    return data_type{positive_tests, outbreak_detected};
  }

  static real_type compare_data_incidence(const real_type time,
                                          const real_type * state,
                                          const data_type& data,
                                          const shared_state& shared,
                                          internal_state& internal,
                                          rng_state_type& rng_state) {
    // As in the update function, access the count of outbreaks summed
    // over all herds in a region, this week.
    const size_t n = shared.n_herds + shared.n_regions;
    const auto* outbreak_region_count = state + 4 * n + shared.n_herds;

    // Negative binomial likelihood for each region, then sum these
    // (logged) over all regions
    real_type ll = 0;
    for (size_t i = 0; i < shared.n_regions; ++i) {
      const auto observed = data.positive_tests[i];
      if (!std::isnan(observed)) {
        const auto noise =
          monty::random::exponential_rate(rng_state, 1e6);
        const auto modelled_count = outbreak_region_count[i] + noise;
        // From ?rnbinom:
        //
        // An alternative parametrization (often used in ecology) is by
        // the mean mu (see above), and size, the dispersion parameter,
        // where prob = size/(size+mu). The variance is mu + mu^2/size
        // in this parametrization.
        //                                         data      "size"             "mu"            log
        ll += monty::density::negative_binomial_mu(observed, shared.dispersion, modelled_count, true);
      }
    }
    return ll;
  }

  static real_type compare_data_survival(const real_type time,
                                         const real_type * state,
                                         const data_type& data,
                                         const shared_state& shared,
                                         internal_state& internal,
                                         rng_state_type& rng_state) {
    real_type ll = 0;

    const size_t n = shared.n_herds + shared.n_regions;
    const real_type* outbreak = state + 4 * n;
    const real_type eps = 1e-6;
    for (size_t i = 0; i < shared.n_regions; ++i) {
      const auto observed = data.outbreak_detected[i];
      if (!std::isnan(observed)) {
        const size_t i_start = shared.region_start[i];
        const size_t i_end = shared.region_start[i + 1];

        // Look across every herd in this region, and see if any of them
        // have detected an outbreak.
        const bool i_outbreak = std::any_of(outbreak + i_start, outbreak + i_end, [](auto v) { return v > 0; });

        // Expressed as a special case of a binomial with n = 1, following Wikipedia:
        // https://en.wikipedia.org/wiki/Bernoulli_distribution#Properties
        //
        // This could be simplified a bit probably but the logs remain.
        const real_type p = nudge(i_outbreak, eps);
        const real_type k = observed;
        ll += k * std::log(p) + (1 - k) * std::log(1 - p);
      }
    }
    return ll;
  }

  static real_type compare_data(const real_type time,
                                const real_type * state,
                                const data_type& data,
                                const shared_state& shared,
                                internal_state& internal,
                                rng_state_type& rng_state) {
    if (shared.likelihood_choice == INCIDENCE) {
      return compare_data_incidence(time, state, data, shared, internal, rng_state);
    } else if (shared.likelihood_choice == SURVIVAL) {
      return compare_data_survival(time, state, data, shared, internal, rng_state);
    } else {
      return NA_REAL;
    }
  }

};
