package distributions;

public class Cholesky {
  public static double[][] getL(double[][] A) {
    int n = A.length;
    double[][] L = new double[n][n];

    for (int i=0; i<n; i++) {
      for (int j=0; j<=i; j++) {

        if (i == j) {
          double x=0; // TODO
          for (int k=0; k<i; k++) {
            x += Math.pow(L[i][k], 2.0);
          }
          L[i][i] = Math.sqrt(A[i][i] - x);
        } else {
          double x=0;
          for (int k=0; k<j; k++) {
            x += L[i][k] * L[j][k];
          }
          L[i][j] = (A[i][j] - x) / L[j][j];
        }

      }
    }

    return L;
  }
}
