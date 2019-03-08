#ifndef UTILS_HPP
#define UTILS_HPP

#include <vector>
#include <nupic/types/Sdr.hpp>

void setup();

std::vector<unsigned char> getTrainImage(unsigned int idx);

unsigned int getTrainLabel(unsigned int idx);

std::vector<unsigned int> getTrainIdxs();

std::vector<unsigned char> getTestImage(unsigned int idx);

unsigned int getTestLabel(unsigned int idx);

std::vector<unsigned int> getTestIdxs();

std::vector<nupic::UInt> getSDRDimensions(nupic::SDR self);
#endif // end ifndef UTILS_HPP
