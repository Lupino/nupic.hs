/* ---------------------------------------------------------------------
 * Copyright (C) 2018, David McDougall.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Affero Public License for more details.
 *
 * You should have received a copy of the GNU Affero Public License
 * along with this program.  If not, see http://www.gnu.org/licenses.
 * ----------------------------------------------------------------------
 */

/**
 * Solving the MNIST dataset with Spatial Pooler.
 *
 * This consists of a simple black & white image encoder, a spatial pool, and an
 * SDR classifier.  The task is to recognise images of hand written numbers 0-9.
 * This should score at least 95%.
 */

#include <algorithm>
#include <cstdint> //uint8_t
#include <iostream>
#include <vector>

#include <nupic/algorithms/SpatialPooler.hpp>
#include <nupic/algorithms/SDRClassifier.hpp>
#include <nupic/utils/SdrMetrics.hpp>

#include <mnist/mnist_reader.hpp> // MNIST data itself + read methods, namespace mnist::

namespace nupic {

using namespace std;
using namespace nupic;

using nupic::algorithms::spatial_pooler::SpatialPooler;
using nupic::algorithms::sdr_classifier::SDRClassifier;
using nupic::types::ClassifierResult;

class MNIST {

  private:
    SpatialPooler sp;
    SDR input;
    SDR columns;
    SDRClassifier clsr;
    mnist::MNIST_dataset<std::vector, std::vector<uint8_t>, uint8_t> dataset;

  public:
    UInt verbosity = 1;
    const UInt train_dataset_iterations = 1u;

    void setup(string path);
    void train();
    void test();

};  // End class MNIST
}   // End namespace examples

