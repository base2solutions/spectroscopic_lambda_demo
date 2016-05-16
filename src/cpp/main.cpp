#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <functional>
#include <math.h>
#include <iomanip>

using namespace std;

string build_output_filename(const std::string &base, long index) {
  stringstream ss;
  ss << base << "_" << index << ".spr";
  return ss.str();  
}

int main (int argv, char** argc) {
  long num_files, starting_wavelength, num_wavelengths;

  cout << "How many spectroscopic samples to create: ";
  cin >> num_files;

  cout << "Starting wavelength (nm): ";
  cin >> starting_wavelength;

  cout << "Number of wavelengths: ";
  cin >> num_wavelengths;

  for (long i = 0; i < num_files; ++i) {
    string output_name = build_output_filename("./output/sample", i);
    cout << "Building spectroscopic data for: " << output_name << endl;

    ofstream output_stream(output_name.c_str(), ios::out);

    for (long j = 0; j < num_wavelengths; ++j) {
      output_stream << fixed << starting_wavelength + j << " " << setprecision(3) << ((double) rand() / (RAND_MAX)) << std::endl;
    }

    output_stream.close();
  }

  string output_name = "./output/sample.spr";

  ofstream output_stream(output_name.c_str(), ios::out);

  for (long j = 0; j < num_wavelengths; ++j) {
    output_stream << fixed << starting_wavelength + j << " " << setprecision(3) << ((double) rand() / (RAND_MAX)) << std::endl;
  }

  output_stream.close();

return 0;
}
