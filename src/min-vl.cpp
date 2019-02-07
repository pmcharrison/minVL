#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
float mod(float x, int base) {
  return x - floor(x / base) * base;
}

// Get voice-leading element distance
//
// Computes the voice-leading distance between two
// elements <e1> and <e2> given <elt_type>. <elt_type>
// can be "pc", in which case the distance
// is computed between two pitch classes, or
// "pitch", in which cases distance is computed
// between two pitches.
// [[Rcpp::export]]
double get_vl_elt_distance(
    double e1,
    double e2,
    String elt_type
) {
  if (elt_type == "pitch") {
    return std::abs(e1 - e2);
  } else if (elt_type == "pc") {
    return std::min(
      std::abs(e1 - e2),
      12 - std::abs(e1 - e2)
    );
  } else {
    stop("Unrecognised value of <elt_type>.");
  }
}

// Get voice-leading ascending distance
//
// Returns the ascending distance from <e1> to <e2>,
// after Tymoczko (2006): the smallest nonnegative real
// number x such that, if p is a pitch with label e1,
// then p + x has label e2. <elt-type> may be either
// "pitch" or "pc; it denotes whether <e1>
// and <e2> are pitches or pitch classes.
// Vectorised over <e2>.
// [[Rcpp::export]]
std::vector<double> get_vl_ascending_distance(
    double e1,
    std::vector<double> e2,
    String elt_type
) {
  int n = e2.size();
  std::vector<double> res(n);
  if (elt_type == "pitch") {
    for (int i = 0; i < n; i ++) {
      res[i] = e2[i] - e1;
    }
  } else if (elt_type == "pc") {
    for (int i = 0; i < n; i ++) {
      res[i] = mod(e2[i] - e1, 12);
    }
  } else stop("Unrecognised <elt_type");
  return res;
}

//' Get voice-leading set distance
//'
//' Computes the voice-leading distance between two
//' ordered sets of the same length, after \insertRef{Tymoczko2006}{minVL}.
//' @param s1 First set (numeric vector)
//' @param s2 Second set (numeric vector)
//' @param elt_type Element type; can be "pitch" for MIDI pitches
//' or "pc" for pitch classes (character scalar)
//' @param norm Norm to use; can be "euclidean", "taxicab", or "infinity"
//' (character scalar)
//' @return Voice-leading distance between the two sets (numeric scalar)
//' @export
// [[Rcpp::export]]
double vl_dist(
    std::vector<double> s1,
    std::vector<double> s2,
    String elt_type = "pc",
    String norm = "taxicab"
) {
  int n = s1.size();
  int m = s2.size();
  if (n != m) {
    stop("vl_dist() requires each input set to have the same size");
  }
  double distances[n];
  for (int i = 0; i < n; i ++) {
    distances[i] = get_vl_elt_distance(
      s1[i], s2[i], elt_type
    );
  }
  double res = 0;
  if (norm == "euclidean") {
    for (int i = 0; i < n; i ++) {
      res += std::pow(distances[i], 2);
    }
    res = sqrt(res);
  } else if (norm == "taxicab") {
    for (int i = 0; i < n; i ++) {
      res += distances[i];
    }
  } else if (norm == "infinity") {
    for (int i = 0; i < n; i ++) {
      if (res < distances[i]) {
        res = distances[i];
      }
    }
  } else stop("Unrecognised norm");
  return res;
}

struct MyComparator
{
  const std::vector<double> & value_vector;

  MyComparator(const std::vector<double> & val_vec):
    value_vector(val_vec) {}

  bool operator()(int i1, int i2)
  {
    return value_vector[i1] < value_vector[i2];
  }
};

// [[Rcpp::export]]
std::vector<double> order_by(std::vector<double> &data,
                             std::vector<double> by) {
  // Make an index
  std::vector<int> index(data.size(), 0);
  for (int i = 0 ; i != index.size() ; i++) {
    index[i] = i;
  }
  // Sort the index according to <by>
  std::sort(
    index.begin(),
    index.end(),
    MyComparator(by)
  );
  // Make a new vector from this index
  int n = data.size();
  std::vector<double> res;
  for (int i = 0; i < n; i ++) {
    res.push_back(data[index[i]]);
  }
  return res;
}

// [[Rcpp::export]]
double add_dist(double prev_dist, double new_pair_dist, String norm) {
  if (norm == "taxicab") {
    return prev_dist + new_pair_dist;
  } else if (norm == "infinity") {
    return std::max(prev_dist, new_pair_dist);
  } else if (norm == "euclidean") {
    return sqrt(pow(prev_dist, 2.0) + pow(new_pair_dist, 2));
  } else {
    stop("Unrecognised norm");
  }
}

std::vector<std::vector<double> > get_vl_dist_matrix(
    const std::vector<double> &a,
    const std::vector<double> &b_tmp,
    const String &elt_type,
    const String &norm
) {
  int height = a.size();
  int width = b_tmp.size();

  std::vector<std::vector<double> > vl_dist(height, std::vector<double>(width, 0));

  for (int i = 0; i < height; i ++) {
    for (int j = 0; j < width; j ++) {
      float prev_dist; // best voice-leading distance for previous neighbours
      if (i == 0 && j == 0) {
        prev_dist = 0;
      } else if (i == 0) {
        prev_dist = vl_dist[0][j - 1];
      } else if (j == 0) {
        prev_dist = vl_dist[i - 1][0];
      } else {
        prev_dist = std::min(vl_dist[i - 1][j - 1],
                             std::min(vl_dist[i][j - 1],
                                      vl_dist[i - 1][j]));
      }
      double new_pair_dist = get_vl_elt_distance(a[i], b_tmp[j], elt_type);
      vl_dist[i][j] = add_dist(prev_dist, new_pair_dist, norm);
    }
  }
  return vl_dist;
}

double extract_minimal_vl_dist(const std::vector<std::vector<double> > &vl_dist) {
  int height = vl_dist.size();
  int width = vl_dist[0].size();

  // We get the minimal voice-leading distance by looking in the three cells
  // bordering the bottom-right corner of the matrix.

  return std::min(vl_dist[height - 2][width - 2],
                  std::min(vl_dist[height - 1][width - 2],
                           vl_dist[height - 2][width - 1]));
}

std::vector<std::vector<double> > extract_minimal_vl(
    std::vector<std::vector<double> > vl_dist_matrix,
    std::vector<double> a,
    std::vector<double> b_tmp
) {
  int height = a.size();
  int width = b_tmp.size();

  std::vector<std::vector<double> > vl_path(2, std::vector<double>(0));

  // Initialise starting position to the bottom right corner of the matrix
  int i = height - 1;
  int j = width - 1;

  while (i > 0 || j > 0) {
    std::vector<int> next_i;
    std::vector<int> next_j;
    std::vector<double> next_dist;

    if (i > 0 && j > 0) {
      next_i.push_back(i - 1);
      next_j.push_back(j - 1);
      next_dist.push_back(vl_dist_matrix[i - 1][j - 1]);
    }
    if (i > 0) {
      next_i.push_back(i - 1);
      next_j.push_back(j);
      next_dist.push_back(vl_dist_matrix[i - 1][j]);
    }
    if (j > 0) {
      next_i.push_back(i);
      next_j.push_back(j - 1);
      next_dist.push_back(vl_dist_matrix[i][j - 1]);
    }

    int best_i;
    int best_j;
    float best_dist;

    int n = next_i.size();

    for (int k = 0; k < n; k ++) {
      if (k == 0 || next_dist[k] < best_dist) {
        best_i = next_i[k];
        best_j = next_j[k];
        best_dist = next_dist[k];
      }
    }
    i = best_i;
    j = best_j;
    vl_path[0].push_back(i);
    vl_path[1].push_back(j);

    if (i < 0 || j < 0) {
      stop("<i> and <j> should never go below zero.");
    }
  }
  // Reverse the voice-leading paths
  std::reverse(vl_path[0].begin(), vl_path[0].end());
  std::reverse(vl_path[1].begin(), vl_path[1].end());

  // Substitute with pitch/pitch classes
  int l = vl_path[0].size();
  for (int x = 0; x < l; x ++) {
    vl_path[0][x] = a[vl_path[0][x]];
    vl_path[1][x] = b_tmp[vl_path[1][x]];
  }

  return vl_path;
}

//' Get minimal voice-leading
//'
//' Computes the minimal voice-leading between two sets of pitches or pitch classes, using the polynomial-time algorithm described in Tymoczko (2006).
//' @param s1 The first set to be compared; numeric vector, with each number corresponding to either a pitch or a pitch class. Duplicates are permitted, and they will be retained. Order does not matter.
//' @param s2 The second set to be compared; see \code{s1}.
//' @param elt_type Can be either \code{pitch} or \code{pc}; determines whether \code{s1} and \code{s2} are interpreted as pitches or pitch classes.
//' @param norm Can be either \code{euclidean}, \code{taxicab}, or \code{infinity}. Each of these identify different norms.
//' @return A list of three values: 1) the minimal voice-leading distance; 2) the corresponding start voicing; 3) the corresponding end voicing.
//' @references
//' \insertRef{Tymoczko2006}{minVL}
//' @export
// [[Rcpp::export]]
List min_vl(
  NumericVector s1,
  NumericVector s2,
  String elt_type = "pc",
  String norm = "taxicab"
) {
  // std::vector<double> a = as<std::vector<double> >(s1);
  // std::vector<double> b = as<std::vector<double> >(s2);
  //
  std::vector<double> a;
  std::vector<double> b;

  // For computational efficiency, we make sure that <b>
  // is assigned to the shortest vector out of <s1> and <s2>.

  bool reverse;

  if (s1.size() < s2.size()) {
    a = as<std::vector<double> >(s2);
    b = as<std::vector<double> >(s1);
    reverse = true;
  } else {
    a = as<std::vector<double> >(s1);
    b = as<std::vector<double> >(s2);
    reverse = false;
  }

  // Get the ascending voice-leading distance for each element
  // of <a> from the first element of <a>
  std::vector<double> a_vl_ascending_distance = get_vl_ascending_distance(
    a[0], a, elt_type
  );

  // Sort a in order of increasing a_ascending distance
  a = order_by(a, a_vl_ascending_distance);

  // Append a_0 to a
  a.push_back(a[0]);

  // If we are dealing with pitch classes, we iterate over
  // many inversions

  std::vector<double> b_0_candidates;

  // Get the different candidates for b[0]
  if (elt_type == "pitch") {
    double b_min = *std::min_element(b.begin(), b.end());
    b_0_candidates.push_back(b_min);
  } else if (elt_type == "pc") {
    b_0_candidates = b;
  } else {
    stop("Unrecognised <elt-type>.");
  }
  int b_0_candidates_size = b_0_candidates.size();

  double best_vl_dist;
  std::vector<std::vector<double> > best_vl_dist_matrix;
  std::vector<double> best_b_tmp;

  for (int h = 0; h < b_0_candidates_size; h ++) {
    double b_0 = b_0_candidates[h];
    // Make a copy of b
    std::vector<double> b_tmp = b;

    // Get the ascending voice-leading distance for each element
    // of <b_tmp> from <b_0>
    std::vector<double> b_vl_ascending_distance = get_vl_ascending_distance(
      b_0, b_tmp, elt_type
    );

    // Sort <b_tmp> in order of increasing a_ascending distance
    b_tmp = order_by(b_tmp, b_vl_ascending_distance);

    // Append b_1 to b_tmp
    b_tmp.push_back(b_0);

    std::vector<std::vector<double> > vl_dist_matrix = get_vl_dist_matrix(a, b_tmp, elt_type, norm);
    double vl_dist = extract_minimal_vl_dist(vl_dist_matrix);
    if (h == 0 || vl_dist < best_vl_dist) {
      best_vl_dist = vl_dist;
      best_vl_dist_matrix = vl_dist_matrix;
      best_b_tmp = b_tmp;
    }
  }

  std::vector<std::vector<double> > min_vl = extract_minimal_vl(best_vl_dist_matrix,
                                                                a,
                                                                best_b_tmp);
  // std::vector<double> start;

  List res = List::create(
    best_vl_dist,
    reverse? min_vl[1] : min_vl[0],
    reverse? min_vl[0] : min_vl[1]
  );
  res.names() = CharacterVector::create("dist", "start", "end");
  return res;
}

//' Get minimal voice-leading distance
//'
//' Computes the minimal voice-leading distance between two sets of pitches or pitch classes,
//' using the polynomial-time algorithm described in Tymoczko (2006).
//' @param s1 The first set to be compared; numeric vector,
//' with each number corresponding to either a pitch or a pitch class.
//' Duplicates are permitted, and they will be retained.
//' Order does not matter.
//' @param s2 The second set to be compared; see \code{s1}.
//' @param elt_type Can be either \code{pitch} or \code{pc};
//' determines whether \code{s1} and \code{s2} are interpreted as pitches or pitch classes.
//' @param norm Can be either \code{euclidean}, \code{taxicab},
//' or \code{infinity}. Each of these identify different norms.
//' @return A numeric scalar corresponding to the minimal voice-leading distance.
//' @note \code{\link{min_vl}()} is equivalent to \code{\link{min_vl_dist}()}
//' but also returns the voice leading itself.
//' @note \code{\link{min_vl_dists}()} is equivalent to \code{\link{min_vl_dist}()}
//' but is vectorised over the first argument.
//' @references
//' \insertRef{Tymoczko2006}{minVL}
//' @export
// [[Rcpp::export]]
double min_vl_dist(
    NumericVector s1,
    NumericVector s2,
    String elt_type = "pc",
    String norm = "taxicab") {
  List vl = min_vl(s1, s2, elt_type, norm);
  double res = vl["dist"];
  return(res);
}

//' Get minimal voice-leading set distances
//'
//' Equivalent to \code{\link{min_vl_dist()}} but vectorised.
//' @param s1_list List of numeric vectors corresponding to the
//' first pitch-class sets to be compared,
//' with each number corresponding to either a pitch or a pitch class.
//' Duplicates are permitted, and they will be retained.
//' Order does not matter.
//' @param s2 List of numeric vectors corresponding to the second
//' pitch-class sets to be compared; see \code{s1}.
//' @param elt_type Can be either \code{pitch} or \code{pc};
//' determines whether \code{s1} and \code{s2} are interpreted as pitches or pitch classes.
//' @param norm Can be either \code{euclidean}, \code{taxicab},
//' or \code{infinity}. Each of these identify different norms.
//' @export
// [[Rcpp::export]]
NumericMatrix min_vl_dists(
    List s1_list,
    List s2_list,
    String elt_type = "pc",
    String norm = "taxicab"
) {
  int n1 = s1_list.size();
  int n2 = s2_list.size();
  NumericMatrix res(n1, n2);
  for (int i = 0; i < n1; i ++) {
    for (int j = 0; j < n2; j ++) {
      NumericVector s1 = s1_list[i];
      NumericVector s2 = s2_list[j];
      res(i, j) = min_vl_dist(s1, s2, elt_type, norm);
    }
  }
  return(res);
}

//' @export
// [[Rcpp::export]]
std::vector<List> min_vls (List s1_list,
              NumericVector s2,
              String elt_type = "pc",
              String norm = "taxicab") {
  int n = s1_list.size();
  std::vector<List> res(n);
  for (int i = 0; i < n; i ++) {
    res[i] = min_vl(s1_list[i], s2, elt_type, norm);
  }
  return(res);
}
