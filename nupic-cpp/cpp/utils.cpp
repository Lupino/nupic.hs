#include "utils.hpp"

std::vector<nupic::UInt> getSDRDimensions(nupic::SDR self) {
    return self.dimensions;
}
