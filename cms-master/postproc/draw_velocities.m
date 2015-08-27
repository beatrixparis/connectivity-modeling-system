%% parameters
nest_filename='nest_1_20100101000000.nc';
%number of different colors you want to use plotting the velocities
num_colors = 100;

%% read in the nest data

% open nestfile
ncid = netcdf.open(nest_filename,'NC_NOWRITE');
% Get the values of Longitude from the nestfile
varidLon = netcdf.inqVarID(ncid,'Longitude');
lonAxis = netcdf.getVar(ncid,varidLon);
% Get the values of Latitude from the nestfile
varidLat = netcdf.inqVarID(ncid,'Latitude');
latAxis = netcdf.getVar(ncid,varidLat);
% Get the values of U-velocity from the nestfile
varidLat = netcdf.inqVarID(ncid,'zu');
uvel = netcdf.getVar(ncid,varidLat);
% Get the values of U-velocity from the nestfile
varidLat = netcdf.inqVarID(ncid,'zv');
vvel = netcdf.getVar(ncid,varidLat);
%close nestfile
netcdf.close(ncid);

%there are three options you can plot:
%the U-velocity
%the V-velocity
%the speed
%Choose one of these three options and comment the other two

%% draw U-velocity

% %set land velocities to NaN
% uvel(uvel==2^100)= NaN;
% %calculate min and max value of speed
% minval = min(min(uvel(:,:,1,1)));
% maxval = max(max(uvel(:,:,1,1)));
% %draw U-velocity
% contourlevels = linspace(minval, maxval, num_colors);
% colormap(jet(num_colors));
% contourf(lonAxis,latAxis,uvel(:,:,1,1)',contourlevels,'linestyle','none');
% colorbar;
% axis equal;

%% draw V-velocity

% %set land velocities to NaN
% vvel(vvel==2^100)= NaN;
% %calculate min and max value of speed
% minval = min(min(vvel(:,:,1,1)));
% maxval = max(max(vvel(:,:,1,1)));
% %draw V-velocity
% contourlevels = linspace(minval, maxval,num_colors);
% colormap(jet(num_colors));
% contourf(lonAxis,latAxis,vvel(:,:,1,1)',contourlevels,'linestyle','none');
% colorbar;
% axis equal;

%% draw Speed

%set land velocities to NaN
uvel(uvel==2^100)= NaN;
vvel(vvel==2^100)= NaN;
%calculate speed
speed = sqrt(uvel .* uvel + vvel .* vvel);
%calculate min and max value of speed
minval = min(min(speed(:,:,1,1)));
maxval = max(max(speed(:,:,1,1)));
%draw speed
contourlevels = linspace(minval, maxval,num_colors);
colormap(jet(num_colors));
contourf(lonAxis,latAxis,speed(:,:,1,1)',contourlevels,'linestyle','none');
colorbar;
axis equal;





