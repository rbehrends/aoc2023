all: .FORCE
	dotnet build -c Release -v quiet
clean: .FORCE
	dotnet clean -c Release -v quiet
	dotnet clean -c Debug -v quiet
run%: .FORCE
	dotnet run -c Release -v quiet --project Day$(call variant,$@) input/input$(call num,$@).txt
test%: .FORCE
	dotnet run -c Release -v quiet --project Day$(call variant,$@) input/test$(call num,$@).txt
build%: .FORCE
	dotnet build -c Release -v quiet --project Day$(call variant,$@)

.FORCE:

num=$(shell echo $(1) | sed -e 's/[^0-9]//g')
variant=$(shell echo $(1) | sed -e 's/[^0-9]*//')
