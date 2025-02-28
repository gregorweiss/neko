/*
 Copyright (c) 2021-2023, The Neko Authors
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:

   * Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

   * Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the following
     disclaimer in the documentation and/or other materials provided
     with the distribution.

   * Neither the name of the authors nor the names of its
     contributors may be used to endorse or promote products derived
     from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 POSSIBILITY OF SUCH DAMAGE.
*/

#include <device/device_config.h>
#include <device/hip/check.h>
#include <hip/hip_runtime.h>

#include "filter_kernels.h"

extern "C" {

/** Fortran wrapper for 2nd order smooth_step
 *
 * Compute the smooth step function for a given array.
 * t = clamp((x - edge0) / (edge1 - edge0), 0.0, 1.0);
 * return t^3 * (t * (6.0 * t - 15.0) + 10.0);
 */
void hip_smooth_step(void* x, real* edge0, real* edge1, int* n) {

    const dim3 nthrds(1024, 1, 1);
    const dim3 nblcks(((*n) + 1024 - 1) / 1024, 1, 1);

    hipLaunchKernelGGL(
        HIP_KERNEL_NAME(smooth_step_kernel<real>), nblcks, nthrds, 0,
        (hipStream_t)glb_cmd_queue, (real*)x, *edge0, *edge1, *n);
    HIP_CHECK(hipGetLastError());
}

/** Fortran wrapper for step function
 *
 * Compute the step function for a given array.
 * if (x < edge) return left;
 * else return right;
 *
 * @param x array to apply the step function
 * @param edge threshold value
 * @param left value to return if x < edge
 * @param right value to return if x >= edge
 * @param n size of the array
 */
void hip_step_function(void* x, real* edge, real* left, real* right, int* n) {
    const dim3 nthrds(1024, 1, 1);
    const dim3 nblcks(((*n) + 1024 - 1) / 1024, 1, 1);

    hipLaunchKernelGGL(
        HIP_KERNEL_NAME(step_kernel<real>), nblcks, nthrds, 0,
        (hipStream_t)glb_cmd_queue, (real*)x, *edge, *left, *right, *n);
    HIP_CHECK(hipGetLastError());
}

/** Fortran wrapper for the permeability filter
 *
 * @param x array to apply the permeability filter on
 * @param k_0 lower bound of the permeability
 * @param k_1 upper bound of the permeability
 * @param q parameter
 * @param n size of the array
 */
void hip_permeability(void* x, real* k_0, real* k_1, real* q, int* n) {
    const dim3 nthrds(1024, 1, 1);
    const dim3 nblcks(((*n) + 1024 - 1) / 1024, 1, 1);

    hipLaunchKernelGGL(
        HIP_KERNEL_NAME(permeability_kernel<real>), nblcks, nthrds, 0,
        (hipStream_t)glb_cmd_queue, (real*)x, *k_0, *k_1, *q, *n);

    HIP_CHECK(hipGetLastError());
}
}
