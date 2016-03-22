################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
CPP_SRCS += \
../c_src/erl_midi.cpp 

OBJS += \
./c_src/erl_midi.o 

CPP_DEPS += \
./c_src/erl_midi.d 


# Each subdirectory must supply rules for building sources it contributes
c_src/%.o: ../c_src/%.cpp
	@echo 'Building file: $<'
	@echo 'Invoking: GCC C++ Compiler'
	g++ -I/opt/local/lib/erlang/usr/include -O3 -Wall -c -fmessage-length=0 -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -o"$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


