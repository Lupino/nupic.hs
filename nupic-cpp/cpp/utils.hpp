#ifndef UTILS_HPP
#define UTILS_HPP

#include <vector>
#include <nupic/types/Sdr.hpp>
#include <nupic/algorithms/BacktrackingTM.hpp>
#include <nupic/encoders/ScalarEncoder.hpp>

void setup();

std::vector<unsigned char> getTrainImage(unsigned int idx);

unsigned int getTrainLabel(unsigned int idx);

std::vector<unsigned char> getTestImage(unsigned int idx);

unsigned int getTestLabel(unsigned int idx);

std::vector<unsigned int> sdr_dimensions(nupic::SDR self);

float * backTM_predict(nupic::algorithms::backtracking_tm::BacktrackingTM &backTM, unsigned int nStep);

nupic::encoders::ScalarEncoder * scalarEncoder_new(
    nupic::Real64 minimum, nupic::Real64 maximum,
    bool clipInput, bool periodic,
    nupic::UInt activeBits,
    nupic::Real sparsity,
    nupic::UInt size,
    Real64 radius, Real64 resolution);

#endif // end ifndef UTILS_HPP
