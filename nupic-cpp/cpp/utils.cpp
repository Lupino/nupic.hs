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

nupic::encoders::ScalarEncoder * scalarEncoder_new(
    nupic::Real64 minimum, nupic::Real64 maximum,
    bool clipInput, bool periodic,
    nupic::UInt activeBits,
    nupic::Real sparsity,
    nupic::UInt size,
    Real64 radius, Real64 resolution) {

  nupic::encoders::ScalarEncoderParameters params;
  params.minimum = minimum;
  params.maximum = maximum;
  params.clipInput = clipInput;
  params.periodic = periodic;
  params.activeBits = activeBits;
  params.sparsity = sparsity;
  params.size = size;
  params.radius = radius;
  params.resolution = resolution;
  return new nupic::encoders::ScalarEncoder( params );
}
