################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
CC_SRCS += \
../src/epapi_base.cc \
../src/epapi_msg.cc \
../src/epapi_pkt.cc \
../src/epapi_term.cc 

OBJS += \
./src/epapi_base.o \
./src/epapi_msg.o \
./src/epapi_pkt.o \
./src/epapi_term.o 

CC_DEPS += \
./src/epapi_base.d \
./src/epapi_msg.d \
./src/epapi_pkt.d \
./src/epapi_term.d 


# Each subdirectory must supply rules for building sources it contributes
src/%.o: ../src/%.cc
	@echo 'Building file: $<'
	@echo 'Invoking: GCC C++ Compiler'
	g++ -I"/home/jldupont/workspace/epapi/trunk/project/lib/include" -O3 -Wall -c -fmessage-length=0 -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -o"$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


