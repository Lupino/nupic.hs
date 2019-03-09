#ifndef UTILS_HPP
#define UTILS_HPP

#include <vector>
#include <nupic/types/Sdr.hpp>
#include <nupic/algorithms/SpatialPooler.hpp>
#include <nupic/algorithms/SDRClassifier.hpp>

void setup();

std::vector<unsigned char> getTrainImage(unsigned int idx);

unsigned int getTrainLabel(unsigned int idx);

std::vector<unsigned char> getTestImage(unsigned int idx);

unsigned int getTestLabel(unsigned int idx);

std::vector<nupic::UInt> getSDRDimensions(nupic::SDR self);

void spatialPooler_save(nupic::algorithms::spatial_pooler::SpatialPooler sp, std::string path);
void spatialPooler_load(nupic::algorithms::spatial_pooler::SpatialPooler &sp, std::string path);
void sdrClassifier_save(nupic::algorithms::sdr_classifier::SDRClassifier clsr, std::string path);
void sdrClassifier_load(nupic::algorithms::sdr_classifier::SDRClassifier &clsr, std::string path);
#endif // end ifndef UTILS_HPP
