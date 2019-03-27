#include "utils.hpp"
#include <mnist/mnist_reader.hpp> // MNIST data itself + read methods, namespace mnist::

std::vector<unsigned int> sdr_dimensions(nupic::SDR self) {
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

float * backTM_predict(nupic::algorithms::backtracking_tm::BacktrackingTM &backTM, unsigned int nStep) {
  std::shared_ptr<float> pred = backTM.predict(nStep);
  return pred.get();
}
