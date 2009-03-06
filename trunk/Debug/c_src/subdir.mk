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
	g++ -I/usr/local/lib/erlang/usr/include -I/usr/local/lib/erlang/lib/erl_interface-3.5.9/include -I/System/Library/Frameworks/AudioUnit.framework/Headers -I/System/Library/Frameworks/AudioToolbox.framework/Headers -I/System/Library/Frameworks/CoreServices.framework/Headers -O0 -g3 -Wall -c -fmessage-length=0 -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -o"$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


