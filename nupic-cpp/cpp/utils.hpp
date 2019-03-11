#ifndef UTILS_HPP
#define UTILS_HPP

#include <vector>
#include <nupic/types/Sdr.hpp>
#include <nupic/algorithms/BacktrackingTMCpp.hpp>

void setup();

std::vector<unsigned char> getTrainImage(unsigned int idx);

unsigned int getTrainLabel(unsigned int idx);

std::vector<unsigned char> getTestImage(unsigned int idx);

unsigned int getTestLabel(unsigned int idx);

std::vector<unsigned int> sdr_dimensions(nupic::SDR self);

float * backTM_predict(nupic::algorithms::backtracking_tm::BacktrackingTMCpp &backTM, unsigned int nStep);

#endif // end ifndef UTILS_HPP
