%% parameters
nest_filename='nest_1_20100101000000.nc';
traj_filename='traj_file_1';

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

%open trajectory file
data=load(traj_filename);
%sort the particles
[tmp,I] = sort(data(:,2));
data = data(I,:);
[tmp,I] = sort(data(:,1));
data = data(I,:);
%get data from particle file
id=data(:,1);
time=data(:,3);
lon=data(:,4);
lat=data(:,5);
depth=data(:,6);
%get the start and end of each trajectory
starts = find(time==0);
ends=[starts(2:end)-1; length(lon)];
%number of trajectories
num_traj=length(starts);
%different color for each trajectory
colors=jet(num_traj);

%% plot all the trajectories

hold on;

for i=1:num_traj

   nums=starts(i):ends(i);
 
   %plot(lon(nums), lat(nums),'color',colors(i,:));
   plot(lon(nums), lat(nums),'color','red');
 
end

hold off;

%print title
title(['Plotted ',num2str(num_traj),' trajectories']);