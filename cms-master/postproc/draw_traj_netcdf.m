%% parameters
nest_filename='nest_1_20100101000000.nc';
traj_filename='traj_file_1.nc';

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
varidU = netcdf.inqVarID(ncid,'zu');
uvel = netcdf.getVar(ncid,varidU);
%close nestfile
netcdf.close(ncid);

%% draw the land

%layer of depth. layer = 1 is the surface.
layer=1;
%divide velocities in land(value=1) and water(value=0)
%velocity of land is 2^100
mask=squeeze(uvel(:,:,layer,1));
mask(mask<2^100)=0;
mask(mask==2^100)= 1;
%draw the land and water
contourf(lonAxis,latAxis,mask',[0.5 0.5],'linestyle','none');
%color of the land in rgb color model
colormap([0.75 0.75 0.75])


%% read in the trajectory data

% open trajectory file
ncid = netcdf.open(traj_filename,'NC_NOWRITE');
% Get the values of time
varidTime = netcdf.inqVarID(ncid,'time');
time = netcdf.getVar(ncid,varidTime);
% Get the values of Longitude
varidLon = netcdf.inqVarID(ncid,'lon');
lon = netcdf.getVar(ncid,varidLon);
% Get the values of Latitude
varidLat = netcdf.inqVarID(ncid,'lat');
lat = netcdf.getVar(ncid,varidLat);
% Get the values of depths
varidDepth = netcdf.inqVarID(ncid,'depth');
depth = netcdf.getVar(ncid,varidDepth);
% Get the values of status
varidStatus = netcdf.inqVarID(ncid,'exitcode');
status = netcdf.getVar(ncid,varidStatus);
% Get the values of release date (in julian)
varidrel = netcdf.inqVarID(ncid,'releasedate');
release = netcdf.getVar(ncid,varidrel);
%close nestfile
netcdf.close(ncid);

lat(lat>999) = NaN;
lon(lon>999) = NaN;

%number of trajectories
num_traj = size(lat,2);

%different color for each trajectory
colors=jet(num_traj);

%% plot all the trajectories

hold on;
for i=1:num_traj
    plot(lon(:,i), lat(:,i),'color','green');
end
hold off;

%print title
title(['Plotted ',num2str(num_traj),' trajectories']);
axis equal;