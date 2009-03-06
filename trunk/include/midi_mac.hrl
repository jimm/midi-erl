%% Author: jakob
%% Created: 5 mar 2009
%% Description: constants for mac midi and core audio

-record('ComponentDescription',
	{componentType = 0,	% A unique 4-byte code indentifying the command set
	 componentSubType = 0, % Particular flavor of this instance
	 componentManufacturer = 0, % Vendor indentification
	 componentFlags = 0, % 8 each for Component,Type,SubType,Manuf/revision
	 componentFlagsMask = 0 % Mask for specifying which flags to consider in search, zero during registration
	}).
