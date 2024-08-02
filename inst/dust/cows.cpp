#include <dust2/common.hpp>
#include <numeric>

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

// [[dust2::class(cows)]]
// [[dust2::time_type(discrete)]]
class cows {
public:
  cows() = delete;

  using real_type = double;

  struct shared_state {
    size_t n_herds;
    size_t n_regions;
    real_type gamma;
    real_type sigma;
    real_type beta;
    real_type alpha;
    real_type time_test;
    real_type n_test;
    std::vector<size_t> region_start;
    std::vector<size_t> herd_to_region_lookup;
    std::vector<real_type> p_region_export;
    std::vector<real_type> p_cow_export;
    std::vector<real_type> n_cows_per_herd;
    std::vector<real_type> movement_matrix;
    real_type start_count;
    size_t start_herd;
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
  };

  struct data_type {
    real_type incidence;
  };

  using rng_state_type = mcstate::random::generator<real_type>;

  static size_t size_state(const shared_state& shared) {
    return 4 * (shared.n_herds + shared.n_regions);
  }

  static void initial(real_type time,
                      real_type dt,
                      const shared_state& shared,
                      internal_state& internal,
                      rng_state_type& rng_state,
                      real_type * state_next) {
    // Start by zeroing everything
    std::fill(state_next, state_next + size_state(shared), 0);
    // Then fill in susceptibles from the mean herd size
    //
    // Thom: should this be drawn from some distribution?  If so that
    // obviously causes a little grief with the seeding as we can't
    // absolutely guarantee that there enough cows to infect.
    const size_t n = shared.n_herds + shared.n_regions;
    auto *S = state_next;
    auto *I = state_next + 2 * n;
    std::copy(shared.n_cows_per_herd.begin(), shared.n_cows_per_herd.end(), S);
    // Seed the infections into the I class
    I[shared.start_herd] = shared.start_count;
    S[shared.start_herd] -= shared.start_count;

    sum_over_regions(S, shared.n_herds, shared.n_regions, shared.region_start);
    sum_over_regions(I, shared.n_herds, shared.n_regions, shared.region_start);
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

    real_type* S_next = state_next;
    real_type* E_next = state_next + n;
    real_type* I_next = state_next + 2 * n;
    real_type* R_next = state_next + 3 * n;

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
      for (size_t j = i_start; j < i_end; ++j) {
        const real_type lambda = internal.N[j] == 0 ? 0 :
          (shared.beta * (I[j] / internal.N[j] + shared.alpha * (tot_I - I[j]) / (tot_N - internal.N[j])));
        const real_type p_SE = 1 - std::exp(-lambda * dt); // S to E
        const real_type n_SE = mcstate::random::binomial<real_type>(rng_state, S[j], p_SE);
        const real_type n_EI = mcstate::random::binomial<real_type>(rng_state, E[j], p_EI);
        const real_type n_IR = mcstate::random::binomial<real_type>(rng_state, I[j], p_IR);
        S_next[j] = S[j] - n_SE;
        E_next[j] = E[j] + n_SE - n_EI;
        I_next[j] = I[j] + n_EI - n_IR;
        R_next[j] = R[j] + n_IR;
      }
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
    for (size_t i = 0; i < shared.n_herds; ++i) {
      const auto j = shared.herd_to_region_lookup[i];
      const auto export_cows = mcstate::random::random_real<real_type>(rng_state) < shared.p_region_export[j] * dt;
      if (export_cows) {
        const auto p_cow_export = shared.p_cow_export[j] * dt;
        internal.export_S[i] = mcstate::random::binomial<real_type>(rng_state, S_next[i], p_cow_export);
        internal.export_E[i] = mcstate::random::binomial<real_type>(rng_state, E_next[i], p_cow_export);
        internal.export_I[i] = mcstate::random::binomial<real_type>(rng_state, I_next[i], p_cow_export);
        internal.export_R[i] = mcstate::random::binomial<real_type>(rng_state, R_next[i], p_cow_export);
      }
    }

    const auto state_travel_allowed = time < shared.time_test;
    for (size_t i_src = 0; i_src < shared.n_herds; ++i_src) {
      const size_t export_N = internal.export_S[i_src] + internal.export_E[i_src] + internal.export_I[i_src] + internal.export_R[i_src];
      if (export_N > 0) {
        const size_t i_region_src = shared.herd_to_region_lookup[i_src];
        const auto p = shared.movement_matrix.begin() + i_region_src * shared.n_regions;
        const real_type u1 = mcstate::random::random_real<real_type>(rng_state);
        const size_t i_region_dst = std::distance(p, std::upper_bound(p, p + shared.n_regions, u1));
        const auto within_region = i_region_src == i_region_dst;
        const real_type u2 = mcstate::random::random_real<real_type>(rng_state);
        const size_t i_dst = shared.region_start[i_region_dst] + std::floor(u2 * (shared.region_start[i_region_dst + 1] - shared.region_start[i_region_dst]));
        const bool allow_movement = within_region || state_travel_allowed ||
          mcstate::random::hypergeometric(rng_state, internal.export_I[i_src], export_N - internal.export_I[i_src], std::min(shared.n_test, internal.export_I[i_src])) == 0;
        if (allow_movement) {
          internal.import_S[i_dst] += internal.export_S[i_src];
          internal.import_E[i_dst] += internal.export_E[i_src];
          internal.import_I[i_dst] += internal.export_I[i_src];
          internal.import_R[i_dst] += internal.export_R[i_src];
        }
      }
    }

    for (size_t i = 0; i < shared.n_herds; ++i) {
      S_next[i] = S_next[i] + internal.import_S[i] - internal.export_S[i];
      E_next[i] = E_next[i] + internal.import_E[i] - internal.export_E[i];
      I_next[i] = I_next[i] + internal.import_I[i] - internal.export_I[i];
      R_next[i] = R_next[i] + internal.import_R[i] - internal.export_R[i];
    }

    sum_over_regions(S_next, shared.n_herds, shared.n_regions, shared.region_start);
    sum_over_regions(E_next, shared.n_herds, shared.n_regions, shared.region_start);
    sum_over_regions(I_next, shared.n_herds, shared.n_regions, shared.region_start);
    sum_over_regions(R_next, shared.n_herds, shared.n_regions, shared.region_start);
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

    const real_type time_test = dust2::r::read_real(pars, "time_test", 30);
    const real_type n_test = dust2::r::read_real(pars, "n_test", 30);

    const real_type start_count = dust2::r::read_real(pars, "start_count");
    const size_t start_herd = dust2::r::read_size(pars, "start_herd") - 1;

    const real_type beta = dust2::r::read_real(pars, "beta");
    const real_type gamma = dust2::r::read_real(pars, "gamma");
    const real_type alpha = dust2::r::read_real(pars, "alpha");
    const real_type sigma = dust2::r::read_real(pars, "sigma");

    return shared_state{n_herds, n_regions, gamma, sigma, beta, alpha, time_test, n_test, region_start, herd_to_region_lookup, p_region_export, p_cow_export, n_cows_per_herd, movement_matrix, start_count, start_herd};
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
    return internal_state{N, export_S, export_E, export_I, export_R, import_S, import_E, import_I, import_R};
  }

  // This is the bit that we'll use to do fast parameter updating, and
  // we'll guarantee somewhere that the size does not change.
  static void update_shared(cpp11::list pars, shared_state& shared) {
    shared.beta = dust2::r::read_real(pars, "beta", shared.beta);
    shared.gamma = dust2::r::read_real(pars, "gamma", shared.gamma);
    shared.alpha = dust2::r::read_real(pars, "alpha", shared.alpha);
    shared.sigma = dust2::r::read_real(pars, "sigma", shared.sigma);
  }

  // This is a reasonable default implementation in the no-internal
  // case
  static void update_internal(const shared_state& shared,
                              internal_state& internal) {
  }

  static auto zero_every(const shared_state& shared) {
    return dust2::zero_every_type<real_type>();
  }
};
