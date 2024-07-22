#include <assert.h>
#include <xrt/xrt_bo.h>
#include <xrt/xrt_kernel.h>

#include <array>
#include <iostream>
#include <random>

void wait_for_enter(const std::string &msg) {
  std::cout << msg << std::endl;
  std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
}

int main(int argc, char **args) {
  std::cout << args[1] << std::endl;
  /**
   * @brief how to determine device_index
   * TIP: The device ID can be obtained using the xbutil  command for a specific
   * accelerator.
   */
  unsigned int device_index = 0;
  //   std::string device_name = "0000:3d:00.1";
  std::cout << "Open the device " << device_index << std::endl;
  auto device = xrt::device(device_index);
  std::cout << "device name:  " << device.get_info<xrt::info::device::name>()
            << std::endl;
  std::cout << "device bdf: " << device.get_info<xrt::info::device::bdf>()
            << std::endl;

  std::string xclbin_path(args[1]);
  std::cout << "Load the xclbin " << xclbin_path << std::endl;
  auto xclbin_uuid = device.load_xclbin(xclbin_path);

  //   std::cout << "The xclbin_uuid is " << xclbin_uuid << std::endl;
  // instantiate kernel
  auto krnl = xrt::kernel(device, xclbin_uuid, "sdpmm_min");

  std::cout << "The krnl name is " << krnl.get_name() << std::endl;

  /**
   * @brief generate random test input data and kv data
   */
  std::random_device dev;
  std::mt19937 rng(dev());
  std::uniform_int_distribution<std::mt19937::result_type> rand10(1, 10);

  // kMatrix datawidth is 32 * 16 = 512
  const auto dim_num = 32;
  const auto length_num = 96;

  std::uniform_int_distribution<std::mt19937::result_type> rand_L(1,
                                                                  length_num);

  using kvMatrix_type = uint16_t;

  auto kMatrix = new kvMatrix_type[length_num][dim_num];
  auto kMatrix_out = new kvMatrix_type[length_num][dim_num];

  for (auto i = 0; i < length_num; i++) {
    for (auto j = 0; j < dim_num; j++) {
      kMatrix[i][j] = (i + j) % 32;
      kMatrix_out[i][j] = 0;
    }
  }

  auto vMatrix = new kvMatrix_type[length_num][dim_num];
  auto vMatrix_out = new kvMatrix_type[length_num][dim_num];
  for (auto i = 0; i < length_num; i++) {
    for (auto j = 0; j < dim_num; j++) {
      vMatrix[i][j] = (i + j + 1) % 32;
      vMatrix_out[i][j] = 0;
    }
  }

  const size_t kv_data = dim_num * length_num * 4 / 64;
  // datawidth is 1024
  // the value datawidth is 16 * 32 + 16 * 32 = 1024
  const auto value_num = 32;
  const auto mask_num = 32;
  // const size_t align_num = 4;

  const auto length = 4;
  const size_t all_data = (value_num * 2 + mask_num * 2) * length / 128;
  using pipe_data = struct InputData {
    // uint16_t value[value_num];
    // uint16_t mask[mask_num];
    // uint16_t align[align_num];

    std::array<uint16_t, value_num> value;
    std::array<uint16_t, mask_num> mask;
  };

  std::array<pipe_data, length> input_data;
  std::array<pipe_data, length> output_data;
  // const auto buffer_size =
  //     (value_num * sizeof(uint16_t) + mask_num * sizeof(uint16_t)) * length;
  const auto buffer_size = sizeof(pipe_data) * length;
  std::cout << "pipe buffer_size: " << buffer_size << std::endl;

  // posix_memalign(reinterpret_cast<void**>(&input_data), 4096, buffer_size);
  // posix_memalign(reinterpret_cast<void**>(&output_data), 4096, buffer_size);
  // input_data->value = new uint16_t[value_num];
  // input_data->mask = new uint16_t[mask_num];

  for (auto i = 0; i < length; i++) {
    for (auto j = 0; j < value_num; j++) {
      input_data[i].value[j] = rand10(rng);
      input_data[i].mask[j] = rand_L(rng);
    }
  }

  // sddmm
  auto temp = new kvMatrix_type[length][mask_num];
  for (auto i = 0; i < length; i++) {
    for (auto j = 0; j < mask_num; j++) {
      for (auto m = 0; m < value_num; m++) {
        temp[i][input_data[i].mask[j]] +=
            input_data[i].value[m] * kMatrix[input_data[i].mask[j]][m];
      }
    }
  }

  // sdpmm
  for (auto i = 0; i < length; i++) {
    for (auto j = 0; j < mask_num; j++) {
      for (auto m = 0; m < value_num; m++) {
        output_data[i].value[m] +=
            temp[i][input_data[i].mask[j]] * vMatrix[input_data[i].mask[j]][m];
      }
    }
  }

  std::cout << "ttttttt" << std::endl;

  const auto kvMatrix_buffer_size =
      length_num * dim_num * sizeof(kvMatrix_type);
  std::cout << "kvMatrix_buffer_size: " << kvMatrix_buffer_size << std::endl;

  // allocate buffer on board

  std::cout << 4 << std::endl;
  auto vrbuffer = xrt::bo(device, kvMatrix_buffer_size, krnl.group_id(8));

  std::cout << 3 << std::endl;
  auto kwbuffer = xrt::bo(device, kvMatrix_buffer_size, krnl.group_id(7));

  std::cout << 5 << std::endl;
  auto vwbuffer = xrt::bo(device, kvMatrix_buffer_size, krnl.group_id(9));

  std::cout << 0 << std::endl;
  auto read_buffer = xrt::bo(device, buffer_size, krnl.group_id(4));

  std::cout << 1 << std::endl;
  auto write_buffer = xrt::bo(device, buffer_size, krnl.group_id(5));

  std::cout << 2 << std::endl;
  auto krbuffer = xrt::bo(device, kvMatrix_buffer_size, krnl.group_id(6));

  wait_for_enter("setup ila and [Enter] to continue...");
  // 输入数据传输到 board
  std::cout << "write_buffer size: "
            << read_buffer.size() + krbuffer.size() + vrbuffer.size()
            << std::endl;
  read_buffer.write(input_data.data());
  read_buffer.sync(XCL_BO_SYNC_BO_TO_DEVICE);

  krbuffer.write(kMatrix);
  krbuffer.sync(XCL_BO_SYNC_BO_TO_DEVICE);

  vrbuffer.write(vMatrix);
  vrbuffer.sync(XCL_BO_SYNC_BO_TO_DEVICE);

  const auto input_numberof_lines = length;
  // for vec add 16*32.W
  auto run = krnl(all_data, kv_data, kv_data, input_numberof_lines, read_buffer,
                  write_buffer, krbuffer, kwbuffer, vrbuffer, vwbuffer);
  // auto run = krnl(data_num * 4 / 64, read_buffer, write_buffer);
  run.wait(1000 * 60 * 5);

  // 计算结果从 board read 回 host
  write_buffer.sync(XCL_BO_SYNC_BO_FROM_DEVICE);
  write_buffer.read(output_data.data());

  kwbuffer.sync(XCL_BO_SYNC_BO_FROM_DEVICE);
  kwbuffer.read(kMatrix_out);

  vwbuffer.sync(XCL_BO_SYNC_BO_FROM_DEVICE);
  vwbuffer.read(vMatrix_out);

  // check result
  for (auto j = 0; j < length; j++) {
    std::cout << "batch: " << j << std::endl;
    for (size_t i = 0; i < value_num; i++) {
      // assert(input_data[i] + 47 == output_data[i]);
      std::cout << "input_value:" << input_data[j].value[i]
                << " output_value:" << output_data[j].value[i] << std::endl;

      std::cout << "input_mask:" << input_data[j].mask[i]
                << " output_mask:" << output_data[j].mask[i] << std::endl;
    }
    std::cout << std::endl;
  }

  for (auto i = 0; i < length_num; i++) {
    for (auto j = 0; j < dim_num; j++) {
      std::cout << "k[" << i << "][" << j << "]:" << kMatrix[i][j] << " k_out["
                << i << "][" << j << "]:" << kMatrix_out[i][j] << std::endl;
    }
  }

  std::cout << std::endl;

  for (auto i = 0; i < length_num; i++) {
    for (auto j = 0; j < dim_num; j++) {
      std::cout << "v[" << i << "][" << j << "]:" << vMatrix[i][j] << " v_out["
                << i << "][" << j << "]:" << vMatrix_out[i][j] << std::endl;
    }
  }

  delete[] kMatrix;
  delete[] kMatrix_out;
  delete[] vMatrix;
  delete[] vMatrix_out;
  // for (auto i = 0; i < length; i++) {
  //   delete[] input_data[i].value;
  //   delete[] input_data[i].mask;
  //   delete[] output_data[i].value;
  //   delete[] output_data[i].mask;
  // }
  // delete[] input_data;
  // delete[] output_data;
  // free(input_data);
  // free(output_data);
}