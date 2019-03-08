#include "utils.hpp"
#include <vector>
#include <mnist/mnist_reader.hpp> // MNIST data itself + read methods, namespace mnist::

std::vector<nupic::UInt> getSDRDimensions(nupic::SDR self) {
    return self.dimensions;
}

mnist::MNIST_dataset<std::vector, std::vector<uint8_t>, uint8_t> g_dataset;

void setup() {
  g_dataset = mnist::read_dataset<std::vector, std::vector, uint8_t, uint8_t>(string("mnist-src/")); //from CMake
}

std::vector<unsigned char> getTrainImage(unsigned int idx) {
  const auto image = g_dataset.training_images.at(idx);
  return image;
}

unsigned int getTrainLabel(unsigned int idx) {
  const unsigned int label  = g_dataset.training_labels.at(idx);
  return label;
}

std::vector<unsigned char> getTestImage(unsigned int idx) {
  const auto image = g_dataset.test_images.at(idx);
  return image;
}

unsigned int getTestLabel(unsigned int idx) {
  const unsigned int label  = g_dataset.test_labels.at(idx);
  return label;
}

void saveSpatialPooler(nupic::algorithms::spatial_pooler::SpatialPooler sp, std::string path) {
  ofstream outfile (path);
  sp.save(outfile);
  outfile.flush();
  outfile.close();
}

void loadSpatialPooler(nupic::algorithms::spatial_pooler::SpatialPooler &sp, std::string path) {
  std::ifstream file(path);
  sp.load(file);
  file.close();
}

void saveSDRClassifier(nupic::algorithms::sdr_classifier::SDRClassifier sp, std::string path) {
  ofstream outfile (path);
  sp.save(outfile);
  outfile.flush();
  outfile.close();
}

void loadSDRClassifier(nupic::algorithms::sdr_classifier::SDRClassifier &sp, std::string path) {
  std::ifstream file(path);
  sp.load(file);
  file.close();
}
