#include <assert.h>
#include <xrt/xrt_bo.h>
#include <xrt/xrt_kernel.h>

#include <iostream>

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
  std::cout << "device name:     " << device.get_info<xrt::info::device::name>()
            << "\n";
  std::cout << "device bdf:      " << device.get_info<xrt::info::device::bdf>()
            << "\n";

  std::string xclbin_path(args[1]);
  std::cout << "Load the xclbin " << xclbin_path << std::endl;
  auto xclbin_uuid = device.load_xclbin(xclbin_path);

  //   std::cout << "The xclbin_uuid is " << xclbin_uuid << std::endl;
  // instantiate kernel
  auto krnl = xrt::kernel(device, xclbin_uuid, "sdpmm");

  std::cout << "The krnl name is " << krnl.get_name() << std::endl;

  // kMatrix datawidth is 32 * 16 = 512
  const size_t dim_num = 32;
  const size_t length_num = 32;

  auto kMatrix = new uint16_t[length_num * dim_num];

  // auto kMatrix_out = new uint16_t[length_num][dim_num];
  auto *kMatrix_out = new uint16_t[length_num * dim_num];

  for (size_t i = 0; i < length_num; i++) {
    for (size_t j = 0; j < dim_num; j++) {
      kMatrix[i * length_num + j] = i * j % 16;
      kMatrix_out[i * length_num + j] = 0;
    }
  }

  auto vMatrix = new uint16_t[length_num][dim_num];
  auto vMatrix_out = new uint16_t[length_num][dim_num];
  for (size_t i = 0; i < length_num; i++) {
    for (size_t j = 0; j < dim_num; j++) {
      vMatrix[i][j] = i + j % 16;
      // vMatrix_out[i][j] = 0;
    }
  }

  const size_t kv_data = dim_num * length_num * 4 / 64;
  // datawidth is 1024
  // the value datawidth is 16 * 32 + 16 * 32 = 1024
  const size_t value_num = 32;
  const size_t mask_num = 32;
  // const size_t align_num = 4;

  const int length = 1;
  const size_t all_data = (value_num * 2 + mask_num * 2) * length / 128;
  typedef struct InputData {
    uint16_t value[value_num];
    uint16_t mask[mask_num];
    // uint16_t align[align_num];
  } pipe_data;

  pipe_data *input_data = new pipe_data[length];
  pipe_data *output_data = new pipe_data[length];

  for (int j = 1; j <= length; j++) {
    for (size_t i = 0; i < value_num; i++) {
      input_data[j - 1].value[i] = j * i % 128;
      input_data[j - 1].mask[i] = (2 * i * j) % 128;
      // input_data[j - 1].align[i] = 0;
    }
  }

  auto buffer_size =
      (value_num * sizeof(uint16_t) + mask_num * sizeof(uint16_t)) * length;

  // const size_t data_num = 128;
  // uint32_t input_data[data_num];
  // uint32_t output_data[data_num];
  // for (size_t i = 0; i < data_num; i++) {
  //   input_data[i] = i % 96;
  // }

  // auto buffer_size = data_num * sizeof(uint32_t);

  wait_for_enter("setup ila and [Enter] to continue...");
  // allocate buffer on board
  auto read_buffer = xrt::bo(device, buffer_size, krnl.group_id(3));
  auto write_buffer = xrt::bo(device, buffer_size, krnl.group_id(4));

  auto krbuffer = xrt::bo(device, (length_num * dim_num) * sizeof(uint16_t),
                          krnl.group_id(5));
  auto kwbuffer = xrt::bo(device, (length_num * dim_num) * sizeof(uint16_t),
                          krnl.group_id(6));
  auto vrbuffer = xrt::bo(device, (length_num * dim_num) * sizeof(uint16_t),
                          krnl.group_id(7));
  auto vwbuffer = xrt::bo(device, (length_num * dim_num) * sizeof(uint16_t),
                          krnl.group_id(8));

  // 输入数据传输到 board
  std::cout << "write_buffer size: "
            << read_buffer.size() + krbuffer.size() + vrbuffer.size()
            << std::endl;
  read_buffer.write(input_data);
  read_buffer.sync(XCL_BO_SYNC_BO_TO_DEVICE);

  krbuffer.write(kMatrix);
  krbuffer.sync(XCL_BO_SYNC_BO_TO_DEVICE);

  vrbuffer.write(vMatrix);
  vrbuffer.sync(XCL_BO_SYNC_BO_TO_DEVICE);

  // for vec add 16*32.W
  auto run = krnl(all_data, kv_data, kv_data, read_buffer, write_buffer,
                  krbuffer, kwbuffer, vrbuffer, vwbuffer);
  // auto run = krnl(data_num * 4 / 64, read_buffer, write_buffer);
  run.wait();

  // 计算结果从 board read 回 host
  write_buffer.sync(XCL_BO_SYNC_BO_FROM_DEVICE);
  write_buffer.read(output_data);

  kwbuffer.sync(XCL_BO_SYNC_BO_FROM_DEVICE);
  kwbuffer.read(kMatrix_out);

  vwbuffer.sync(XCL_BO_SYNC_BO_FROM_DEVICE);
  vwbuffer.read(vMatrix_out);

  // check result
  for (int j = 0; j < length; j++) {
    for (size_t i = 0; i < value_num; i++) {
      // assert(input_data[i] + 47 == output_data[i]);
      std::cout << "input_value:" << input_data[j].value[i]
                << " output_value:" << output_data[j].value[i] << std::endl;

      std::cout << "input_mask:" << input_data[j].mask[i]
                << " output_mask:" << output_data[j].mask[i] << std::endl;
    }
    std::cout << std::endl;
  }

  for (size_t i = 0; i < length_num; i++) {
    for (size_t j = 0; j < dim_num; j++) {
      std::cout << "kmatrix[" << i << "][" << j
                << "]:" << kMatrix[i * length_num + j] << " kmatrix_out[" << i
                << "][" << j << "]:" << kMatrix_out[i * length_num + j]
                << std::endl;
    }
  }

  std::cout << std::endl;

  for (size_t i = 0; i < length_num; i++) {
    for (size_t j = 0; j < dim_num; j++) {
      std::cout << "vMatrix[" << i << "][" << j << "]:" << vMatrix[i][j]
                << " vMatrix_out[" << i << "][" << j
                << "]:" << vMatrix_out[i][j] << std::endl;
    }
  }

  delete[] input_data;
  delete[] output_data;
  delete[] kMatrix;
  delete[] kMatrix_out;
  // delete[] kMatrix;
  // delete[] kMatrix_out;
  delete[] vMatrix;
  delete[] vMatrix_out;

  // for (size_t i = 0; i < data_num; i++) {
  //   // assert(input_data[i] + 47 == output_data[i]);
  //   if (input_data[i] + 47 != output_data[i]) {
  //     std::cout << "error: index is " << i << " input: " << input_data[i]
  //               << " output:" << output_data[i] << std::endl;
  //   } else {
  //     std::cout << "index is " << i << " input:" << input_data[i]
  //               << " output:" << output_data[i] << std::endl;
  //   }
  // }
}