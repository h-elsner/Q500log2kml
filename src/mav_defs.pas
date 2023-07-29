unit mav_defs;                                     {MAVlink definitions and variables}

{$mode objfpc}{$H+}

interface

uses
  sysutils, q5_common;

const
  MAVurl='https://github.com/mavlink/c_library_v2/tree/master/common';
{Extract data from following messages}
  MAVmsgX=[0, 1, 22, 24, 30, 32, 33, 65, 74, 87, 105, 141, 147, 245, 253];

{Public functions and procedures}
  function MsgIDtoStr(id: integer): string;
  function CoordFrameToStr(f: integer): string;    {for POSITION_TARGET_GLOBAL_INT}
  function MAVseverity(sv: byte): string;
  function MAVcompID(cid: byte): string;           {MAV_Component_ID}
  function MSenStat(m: longword): string;          {uint32 MAV_SYS_STATUS_SENSOR}
  function GPSfixType(const s:string): string;     {MAVlink GPS fix type to string}
  function MLStoStr(const m: byte): string;        {MAV_LANDED_STATE}
  function MSTtoStr(const m: byte): string;        {Bitleiste MAV_STATE auswerten}
  function MMFtoStr(const m: byte): string;        {Bitleiste MAV_Mode_FLAG auswerten}

{$I language.inc}

implementation

{Message Struktur:
 https://github.com/mavlink/c_library_v2/tree/master/common
      inconsistent !
 https://github.com/YUNEEC/MavlinkLib/blob/master/message_definitions/common.xml
 https://github.com/YUNEEC/MavlinkLib}

function MsgIDtoStr(id: integer): string;
begin
  result:=rsUnknown+' MAV_CMD'+' $'+IntToHex(id, 2)+
          ' ('+IntToStr(id)+')';                   {default}
  case id of
      0:  result:='heartbeat';                     {Supported Msg Länge 9}
      1:  result:='sys_status';                    {Supported Msg Länge 1F}
      2:  result:='system_time';                   {Länge 0B}
      4:  result:='ping';
      5:  result:='change_operator_control';
      6:  result:='change_operator_control_ack';
      7:  result:='auth_key';
      8:  result:='link_node_status';
     11:  result:='set_mode';
     19:  result:='param_ack_transaction';
     20:  result:='param_request_read';
     21:  result:='param_request_list';
     22:  result:='param_value';
     23:  result:='param_set';
     24:  result:='gps_raw_int';                   {Supported Msg Länge 31/32}
     25:  result:='gps_status';                    {Länge 1}
     26:  result:='scaled_imu';
    $1B:  result:='raw_imu';
    $1C:  result:='raw_pressure';
    $1D:  result:='scaled_pressure';
    $1E:  result:='attitude';                      {Länge 1C}
    $1F:  result:='attitude_quaternion';           {Supported Msg Länge 20}
    $20:  result:='local_position_ned';            {Länge 1C}
    $21:  result:='global_position_int';           {Supported Msg Länge 1C}
    $22:  result:='rc_channels_scaled';
    $23:  result:='rc_channels_raw';
    $24:  result:='servo_output_raw';              {Länge 10 oder 15}
    $25:  result:='mission_request_partial_list';
    $26:  result:='mission_write_partial_list';
    $27:  result:='mission_item';
    $28:  result:='mission_request';
    $29:  result:='mission_set_current';
    $2A:  result:='mission_current';
     43:  result:='mission_request_list';
    $2C:  result:='mission_count';                 {Länge 3 oder 5}
    $2D:  result:='mission_clear_all';
    $2E:  result:='mission_item_reached';
    $2F:  result:='mission_ack';
    $30:  result:='set_gps_global_origin';
    $31:  result:='gps_global_origin';
    $32:  result:='param_map_rc';
     51:  result:='mission_request_int';
     52:  result:='mission_changed';

     54:  result:='safety_set_allowed_area';
     55:  result:='safety_allowed_area';
    $3D:  result:='attitude_quaternion_cov';
    $3E:  result:='nav_controller_output';
    $3F:  result:='global_position_int_cov';
    $40:  result:='local_position_ned_cov';
    $41:  result:='rc_channels';                   {Supported Msg Länge 2A}
    $42:  result:='request_data_stream';
    $43:  result:='data_stream';
    $45:  result:='manual_control';                {Länge 0B}
    $46:  result:='rc_channels_override';          {Länge 11}
    $49:  result:='mission_item_int';
    $4A:  result:='vfr_hud';                       {Länge 11}
    $4B:  result:='command_int';
    $4C:  result:='command_long';                  {Länge 20}
    $4D:  result:='command_ack';
    $4E:  result:='command_cancel';                {78: UTC time stamp, Boot time}
    $4F:  result:='command_long_stamped';          {79: not supported anymore}
    $51:  result:='manual_setpoint';
    $52:  result:='set_attitude_target';
    $53:  result:='attitude_target';               {Länge 24}
    $54:  result:='set_position_target_local_ned';
    $55:  result:='position_target_local_ned';     {Länge 33}
    $56:  result:='set_position_target_global_int';
    $57:  result:='position_target_global_int';    {Länge 3}
    $59:  result:='local_position_ned_system_global_offset';
    $5A:  result:='hil_state';
    $5B:  result:='hil_controls';
    $5C:  result:='hil_rc_inputs_raw';
    $5D:  result:='hil_actuator_controls';

    $64:  result:='optical_flow';
    $65:  result:='global_vision_position_estimate';
    $66:  result:='vision_position_estimate';
    $67:  result:='vision_speed_estimate';
    $68:  result:='vicon_position_estimate';
    $69:  result:='highres_imu';                   {Länge 3E}
    $6A:  result:='optical_flow_rad';
    $6B:  result:='hil_sensor';
    $6C:  result:='sim_state';
    $6D:  result:='radio_status';
    $6E:  result:='file_transfer_protocol';
    $6F:  result:='timesync';                      {Länge 0D}
    $70:  result:='camera_trigger';
    $71:  result:='hil_gps';
    $72:  result:='hil_optical_flow';
    $73:  result:='hil_state_quaternion';
    116:  result:='scaled_imu2';
    $75:  result:='log_request_list';
    $76:  result:='log_entry';
    $77:  result:='log_request_data';
    $78:  result:='log_data';
    $79:  result:='log_erase';
    $7A:  result:='log_request_end';
    $7B:  result:='gps_inject_data';
    $7C:  result:='gps2_raw';
    $7D:  result:='power_status';
    $7E:  result:='serial_control';
    $7F:  result:='gps_rtk';
    $80:  result:='gps2_rtk';
    $81:  result:='scaled_imu3';
    $82:  result:='data_transmission_handshake';
    $83:  result:='encapsulated_data';
    $84:  result:='distance_sensor';
    $85:  result:='terrain_request';
    $86:  result:='terrain_data';
    $87:  result:='terrain_check';
    $88:  result:='terrain_report';
    $89:  result:='scaled_pressure2';
    $8A:  result:='att_pos_mocap';
    $8B:  result:='set_actuator_control_target';
    $8C:  result:='actuator_control_target';       {Länge 14}
    $8D:  result:='altitude';                      {Länge 20}
    $8E:  result:='resource_request';
    $8F:  result:='scaled_pressure3';
    $90:  result:='follow_target';
    $92:  result:='control_system_state';
    $93:  result:='battery_status';
    $94:  result:='autopilot_version';             {Länge 34, 48, 4C}
    149:  result:='landing_target';

    162:  result:='fence_status';
    192:  result:='mag_cal_report';

    225:  result:='efi_status';

{MESSAGE IDs 180 - 229: Space for custom messages in
 individual projectname_messages.xml files -->}
(*  201:  result:='sens_power';                    {not know if used}
    202:  result:='sens_MPTT';
    203:  result:='aslctrl_data';
    204:  result:='aslctrl_debug';
    205:  result:='asluav_status';
    206:  result:='ekf_ext';                       {Wind speed and such stuff}
    207:  result:='asl_obctrl';
    208:  result:='sens_atmos';                    {Atmospheric sensors}
    209:  result:='sens_batmon';                   {Battery monitor}
    210:  result:='fw_soaring_data';               {fixed wing...}
    211:  result:='sensorpod_status';
    212:  result:='sens_power_board';
    213:  result:='gsm_link_status';               {LTE too}       *)

    230:  result:='estimator_status';              {Länge 2A}
    $E7:  result:='wind_cov';                      {Länge 20}
    $E8:  result:='gps_input';
    $E9:  result:='gps_rtcm_data';
    $EA:  result:='high_latency';
    $EB:  result:='high_latency2';
    241:  result:='vibration';                     {Länge 14}
    $F2:  result:='home_position';                 {Supported Msg Länge 28 oder 3C}
    $F3:  result:='set_home_position';
    $F4:  result:='message_interval';
    $F5:  result:='extended_sys_state';            {Länge 02}
    $F6:  result:='adsb_vehicle';
    $F7:  result:='collision';
    $F8:  result:='v2_extension';
    $F9:  result:='memory_vect';
    $FA:  result:='debug_vect';
    $FB:  result:='named_value_float';
    $FC:  result:='named_value_int';
    $FD:  result:='statustext';                    {Länge variabel}
    $FE:  result:='debug';
    256:  result:='setup_signing';
    $101: result:='button_change';
    $102: result:='play_tune';
    $103: result:='camera_information';
    $104: result:='camera_settings';
    $105: result:='storage_information';
    $106: result:='camera_capture_status';
    $107: result:='camera_image_captured';         {Länge FC}
    $108: result:='flight_information';            {Supported Msg Länge 1B}
    $109: result:='mount_orientation';             {Länge 20}
    $10A: result:='logging_data';
    $10B: result:='logging_data_acked';
    $10C: result:='logging_ack';
    $10D: result:='video_stream_information';
    $10E: result:='video_stream_status';           {270 len 19}
    $10F: result:='camera_fov_status';             {271 len 52}

    275:  result:='camera_tracking_image_status';  {275 len 31}
    276:  result:='camera_tracking_geo_status';    {276 len 49}

    280:  result:='gimbal_manager_information';
    281:  result:='gimbal_manager_status';
    282:  result:='gimbal_manager_set_attitude';
    283:  result:='gimbal_device_information';
    284:  result:='gimbal_device_set_attitude';
    285:  result:='gimbal_device_attitude_status';
    286:  result:='autopilot_state_for_gimbal_device';
    287:  result:='gimbal_manager_set_pitchyaw';
    288:  result:='gimbal_manager_set_manual_control';
    290:  result:='esc_info';
    291:  result:='esc_status';

    299:  result:='wifi_config_ap';
    300:  result:='protocol_version';              {12C'h not supported anymore}

    301:  result:='ais_vessel';

    310:  result:='uavcan_node_status';
    $137: result:='uavcan_node_info';
    $140: result:='param_ext_request_read';
    $141: result:='param_ext_request_list';
    $142: result:='param_ext_value';               {Länge 95}
    $143: result:='param_ext_set';
    $144: result:='param_ext_ack';                 {Länge 91}
    $14A: result:='obstacle_distance';             {Länge 9E}
    $14B: result:='odometry';
    $14C: result:='trajectory_representation_waypoints';
    $14D: result:='trajectory_representation_bezier';

    336:  result:='cellular_config';

    339:  result:='raw_rpm';
    340:  result:='UTM_global_position';           {154'h}
    350:  result:='debug_float_array';
    360:  result:='orbit_execution_status';

    370:  result:='smart_battery_info';
    373:  result:='generator_status';
    375:  result:='actuator_output_status';
    380:  result:='time_estimate_to_target';
    385:  result:='tunnel';
    390:  result:='onboard_computer_status';
    395:  result:='component_information';
    400:  result:='play_tune v2';
    401:  result:='supported_tunes';

    9000: result:='wheel_distance';                {2328'h}
    9005: result:='winch_status';

   12900: result:='open_drone_id_basic_id';        {3264'h}
   12901: result:='open_drone_id_location';
   12902: result:='open_drone_id_authentication';
   12903: result:='open_drone_id_self_id';
   12904: result:='open_drone_id_system';
   12905: result:='open_drone_id_operator_id';
   12915: result:='open_drone_id_message_pack';
   12918: result:='open_drone_id_arm_status';
   12919: result:='open_drone_id_system_update';
   19920: result:='hygrometer_sensor';
  end;
end;

{https://github.com/mavlink/c_library_v2/blob/master/common/mavlink_msg_position_target_global_int.h}
function CoordFrameToStr(f: integer): string;      {for POSITION_TARGET_GLOBAL_INT}
begin
  result:='';
  case f of
    5:  result:='GLOBAL';
    6:  result:='GLOBAL_RELATIVE_ALT';
    11: result:='GLOBAL_TERRAIN_ALT';
  end;
end;


(*  unused until now
{https://github.com/YUNEEC/MavlinkLib/blob/master/message_definitions/ASLUAV.xml}
function GSMlinkType(sv: byte): string;
begin
  result:='Link type unknown';
  case sv of
    0: result:='No service';
    2: result:='2G (GSM/GRPS/EDGE) link';
    3: result:='3G link (WCDMA/HSDPA/HSPA)';
    4: result:='4G link (LTE)';
  end;
end;   *)

{ENUMs see: https://github.com/mavlink/c_library_v2/blob/master/common/common.h}
function MAVseverity(sv: byte): string;
begin
  result:='';
  case sv of
    0: result:=emcyID;      {System is unusable. This is a "panic" condition}
    1: result:='ALERT';     {Action should be taken immediately. Indicates error
                             in non-critical systems}
    2: result:='CRITICAL';  {Action must be taken immediately. Indicates failure
                             in a primary system}
    3: result:='ERROR';     {Indicates an error in secondary/redundant systems}
    4: result:='WARNING';   {Indicates about a possible future error if this
                             is not resolved within a given timeframe. Example
                             would be a low battery warning}
    5: result:='NOTICE';    {An unusual event has occurred, though not an error
                             condition. This should be investigated for the
                             root cause.}
    6: result:='INFO';      {Normal operational messages. Useful for logging.
                             No action is required for these messages.}
    7: result:='DEBUG';     {Useful non-operational messages that can assist in
                             debugging. These should not occur during normal
                             operation}
  end;
end;

{https://developer.yuneec.com/documentation/125/Supported-mavlink-messages
 https://github.com/YUNEEC/MavlinkLib/blob/master/message_definitions/common.xml}
function MAVcompID(cid: byte): string;             {MAV_Component_ID}
begin
  result:='';
  case cid of
    0: result:='ALL';
    1: result:='AUTOPILOT 1';
    25..86, 88..99: result:='USER '+IntToStr(cid-24);
    87: result:='USER 63 (H520 - CGO-ET?)';
    100..105: result:='CAMERA '+IntToStr(cid-99);
    140..153: result:='SERVO '+IntToStr(cid-139);
    154: result:='GIMBAL 1';
    155: result:='LOG';
    156: result:='ADSB';
    157: result:='OSD';
    158: result:='PERIPHERAL';
    159: result:='QX1_GIMBAL';
    160: result:='FLARM';
    171..175: result:='GIMBAL '+IntToStr(cid-169);
    180: result:='MAPPER';
    190: result:='MISSIONPLANNER';
    195: result:='PATHPLANNER';
    200: result:='IMU';
    201: result:='IMU 2';
    202: result:='IMU 3';
    220: result:='GPS';
    221: result:='GPS 2';
    240: result:='UDP BRIDGE';
    241: result:='UART BRIDGE';
    250: result:='SYSTEM CONTROL';
  end;
end;

{https://github.com/mavlink/c_library_v2/blob/master/common/common.h

1	MAV_SYS_STATUS_SENSOR_3D_GYRO	0x01 3D gyro
2	MAV_SYS_STATUS_SENSOR_3D_ACCEL	0x02 3D accelerometer
4	MAV_SYS_STATUS_SENSOR_3D_MAG	0x04 3D magnetometer
8	MAV_SYS_STATUS_SENSOR_ABSOLUTE_PRESSURE	0x08 absolute pressure
16	MAV_SYS_STATUS_SENSOR_DIFFERENTIAL_PRESSURE	0x10 differential pressure
32	MAV_SYS_STATUS_SENSOR_GPS	0x20 GPS
64	MAV_SYS_STATUS_SENSOR_OPTICAL_FLOW	0x40 optical flow
128	MAV_SYS_STATUS_SENSOR_VISION_POSITION	0x80 computer vision position
256	MAV_SYS_STATUS_SENSOR_LASER_POSITION	0x100 laser based position
512	MAV_SYS_STATUS_SENSOR_EXTERNAL_GROUND_TRUTH	0x200 external ground truth (Vicon or Leica)
1024	MAV_SYS_STATUS_SENSOR_ANGULAR_RATE_CONTROL	0x400 3D angular rate control
2048	MAV_SYS_STATUS_SENSOR_ATTITUDE_STABILIZATION	0x800 attitude stabilization
4096	MAV_SYS_STATUS_SENSOR_YAW_POSITION	0x1000 yaw position
8192	MAV_SYS_STATUS_SENSOR_Z_ALTITUDE_CONTROL	0x2000 z/altitude control
16384	MAV_SYS_STATUS_SENSOR_XY_POSITION_CONTROL	0x4000 x/y position control
32768	MAV_SYS_STATUS_SENSOR_MOTOR_OUTPUTS	0x8000 motor outputs / control
65536	MAV_SYS_STATUS_SENSOR_RC_RECEIVER	0x10000 rc receiver
131072	MAV_SYS_STATUS_SENSOR_3D_GYRO2	0x20000 2nd 3D gyro
262144	MAV_SYS_STATUS_SENSOR_3D_ACCEL2	0x40000 2nd 3D accelerometer
524288	MAV_SYS_STATUS_SENSOR_3D_MAG2	0x80000 2nd 3D magnetometer
1048576	MAV_SYS_STATUS_GEOFENCE	0x100000 geofence
2097152	MAV_SYS_STATUS_AHRS	0x200000 AHRS subsystem health
4194304	MAV_SYS_STATUS_TERRAIN	0x400000 Terrain subsystem health
8388608	MAV_SYS_STATUS_REVERSE_MOTOR	0x800000 Motors are reversed
16777216	MAV_SYS_STATUS_LOGGING	0x1000000 Logging
33554432	MAV_SYS_STATUS_SENSOR_BATTERY	0x2000000 Battery
67108864	MAV_SYS_STATUS_SENSOR_PROXIMITY	0x4000000 Proximity
134217728	MAV_SYS_STATUS_SENSOR_SATCOM	0x8000000 Satellite Communication}

function MSenStat(m: longword): string;            {uint32 MAV_SYS_STATUS_SENSOR}
begin
  result:='';
  if m>0 then begin
    if (m and 1)>0   then result:=result+'3D_GYRO ';
    if (m and 2)>0   then result:=result+'3D_ACCEL ';
    if (m and 4)>0   then result:=result+'3D_MAG ';
    if (m and 8)>0   then result:=result+'ABSOLUTE_PRESSURE ';
    if (m and 16)>0  then result:=result+'DIFFERENTIAL_PRESSURE ';
    if (m and 32)>0  then result:=result+'GPS ';
    if (m and 64)>0  then result:=result+'OPTICAL_FLOW ';
    if (m and 128)>0 then result:=result+'VISION_POSITION ';
    if (m and 256)>0 then result:=result+'LASER_POSITION ';
    if (m and 512)>0 then result:=result+'EXTERNAL_GROUND_TRUTH ';
    if (m and 1024)>0    then result:=result+'ANGULAR_RATE_CONTROL ';
    if (m and 2048)>0    then result:=result+'ATTITUDE_STABILIZATION ';
    if (m and 4096)>0    then result:=result+'YAW_POSITION ';
    if (m and 8192)>0    then result:=result+'Z_ALTITUDE_CONTROL ';
    if (m and 16384)>0   then result:=result+'XY_POSITION_CONTROL ';
    if (m and 32768)>0   then result:=result+'MOTOR_OUTPUTS ';
    if (m and 65536)>0   then result:=result+'RC_RECEIVER ';
    if (m and 131072)>0  then result:=result+'3D_GYRO2 ';
    if (m and 262144)>0  then result:=result+'3D_ACCEL2 ';
    if (m and 524288)>0  then result:=result+'3D_MAG2 ';
    if (m and 1048576)>0   then result:=result+'GEOFENCE ';
    if (m and 2097152)>0   then result:=result+'AHRS ';
    if (m and 4194304)>0   then result:=result+'TERRAIN ';
    if (m and 8388608)>0   then result:=result+'REVERSE_MOTOR ';
    if (m and 16777216)>0  then result:=result+'LOGGING ';
    if (m and 33554432)>0  then result:=result+'BATTERY ';
    if (m and 67108864)>0  then result:=result+'PROXIMITY ';
    if (m and 134217728)>0 then result:=result+'SATCOM ';
  end else result:='0';
end;

{https://mavlink.io/en/messages/common.html#GPS_FIX_TYPE (nicht aktuell!)
 besser hier: https://github.com/mavlink/c_library_v2/tree/master/common
 siehe heartbeat "Verdrehung"

0	GPS_FIX_TYPE_NO_GPS	No GPS connected
1	GPS_FIX_TYPE_NO_FIX	No position information, GPS is connected
2	GPS_FIX_TYPE_2D_FIX	2D position
3	GPS_FIX_TYPE_3D_FIX	3D position
4	GPS_FIX_TYPE_DGPS	DGPS/SBAS aided 3D position
5	GPS_FIX_TYPE_RTK_FLOAT	RTK float, 3D position
6	GPS_FIX_TYPE_RTK_FIXED	RTK Fixed, 3D position
7	GPS_FIX_TYPE_STATIC	Static fixed, typically used for base stations
8	GPS_FIX_TYPE_PPP	PPP, 3D position.}

function GPSfixType(const s:string): string;       {MAVlink GPS fix type to string}
var w: integer;
begin
  result:=rsUnknown;
  w:=StrToIntDef(s, 0);
  case w of
    0:	Result:='No GPS connected';
    1:	Result:='No position information, GPS is connected';
    2:	Result:='2D position';
    3:	Result:='3D position';
    4:	Result:='DGPS/SBAS aided 3D position';
    5:	Result:='RTK float, 3D position';
    6:	Result:='RTK fixed, 3D position';
    7:	Result:='Static fixed, typically used for base stations';
    8:	Result:='PPP, 3D position';
  end;
end;

{https://mavlink.io/en/messages/common.html#MAV_LANDED_STATE
 (nicht aktuell!) besser hier: https://github.com/mavlink/c_library_v2/tree/master/common
  siehe heartbeat "Verdrehung"
  0	MAV_LANDED_STATE_UNDEFINED	MAV landed state is unknown
  1	MAV_LANDED_STATE_ON_GROUND	MAV is landed (on ground)
  2	MAV_LANDED_STATE_IN_AIR	MAV is in air
  3	MAV_LANDED_STATE_TAKEOFF	MAV currently taking off
  4	MAV_LANDED_STATE_LANDING	MAV currently landing  }

function MLStoStr(const m: byte): string;          {MAV_LANDED_STATE}
begin
  case m of
    1: result:='Landed (on ground)';
    2: result:='In air';
    3: result:='Currently taking off';
    4: result:='Currently landing';
  else
    result:='MAV landed state undef';
  end;
end;

{https://mavlink.io/en/messages/common.html#MAV_STATE
 (nicht aktuell!) besser hier: https://github.com/mavlink/c_library_v2/tree/master/common
 siehe heartbeat "Verdrehung"
0	MAV_STATE_UNINIT	Uninitialized system, state is unknown.
bit 0	MAV_STATE_BOOT	        System is booting up.
bit 1	MAV_STATE_CALIBRATING	System is calibrating and not flight-ready.
bit 2	MAV_STATE_STANDBY	System is grounded and on standby. It can be launched any time.
bit 3	MAV_STATE_ACTIVE	System is active and might be already airborne. Motors are engaged.
bit 4	MAV_STATE_CRITICAL	System is in a non-normal flight mode. It can however still navigate.
bit 5	MAV_STATE_EMERGENCY	System is in a non-normal flight mode. It lost control over parts or over the whole airframe. It is in mayday and going down.
bit 6	MAV_STATE_POWEROFF	System just initialized its power-down sequence, will shut down now.
bit 7	MAV_STATE_FLIGHT_TERMINATION	System is terminating itself.

H Plus: MAVstate = 0 wird während Compass Calibration ausgegeben, sollte 2 sein}

function MSTtoStr(const m: byte): string;          {Bitleiste MAV_STATE auswerten}
begin
  result:='MAV_STATE'+suff;
  case m of
    0: result:=result+'UNINIT';
    1: result:=result+'BOOT';
    2: result:=result+'CALIBRATING';
    3: result:=result+'STANDBY';
    4: result:=result+'ACTIVE';
    5: result:=result+'CRITICAL';
    6: result:=result+emcyID;
    7: result:=result+'POWEROFF';
    8: result:=result+'FLIGHT_TERMINATION';
 else
    result:=result+rsUnknown;
 end;
end;

{https://mavlink.io/en/messages/common.html      MAV_MODE_FLAG
(nicht aktuell!) besser hier: https://github.com/mavlink/c_library_v2/tree/master/common
 siehe heartbeat "Verdrehung"

 Flags:
 https://github.com/mavlink/c_library_v2/blob/master/common/common.h

These flags encode the MAV mode. In Heartbeat base-mode +6
Value	Field Name	Description
128	MAV_MODE_FLAG_SAFETY_ARMED	0b10000000 MAV safety set to armed. Motors are enabled / running / can start. Ready to fly. Additional note: this flag is to be ignore when sent in the command MAV_CMD_DO_SET_MODE and MAV_CMD_COMPONENT_ARM_DISARM shall be used instead. The flag can still be used to report the armed state.
64	MAV_MODE_FLAG_MANUAL_INPUT_ENABLED	0b01000000 remote control input is enabled.
32	MAV_MODE_FLAG_HIL_ENABLED	0b00100000 hardware in the loop simulation. All motors / actuators are blocked, but internal software is full operational.
16	MAV_MODE_FLAG_STABILIZE_ENABLED	0b00010000 system stabilizes electronically its attitude (and optionally position). It needs however further control inputs to move around.
8	MAV_MODE_FLAG_GUIDED_ENABLED	0b00001000 guided mode enabled, system flies waypoints / mission items.
4	MAV_MODE_FLAG_AUTO_ENABLED	0b00000100 autonomous mode enabled, system finds its own goal positions. Guided flag can be set or not, depends on the actual implementation.
2	MAV_MODE_FLAG_TEST_ENABLED	0b00000010 system has a test mode enabled. This flag is intended for temporary system tests and should not be used for stable implementations.
1	MAV_MODE_FLAG_CUSTOM_MODE_ENABLED	0b00000001 Reserved for future use.}

function MMFtoStr(const m: byte): string;          {Bitleiste MAV_Mode_FLAG auswerten}
begin
  result:='MAV_MODE_FLAG'+suff;
  if m>0 then begin
    if (m and 1)>0   then result:=result+'CUSTOM_MODE_ENABLED ';
    if (m and 2)>0   then result:=result+'TEST_ENABLED ';
    if (m and 4)>0   then result:=result+'AUTO_ENABLED ';
    if (m and 8)>0   then result:=result+'GUIDED_ENABLED ';
    if (m and 16)>0  then result:=result+'STABILIZE_ENABLED ';
    if (m and 32)>0  then result:=result+'HIL_ENABLED ';
    if (m and 64)>0  then result:=result+'MANUAL_INPUT_ENABLED ';
    if (m and 128)>0 then result:=result+'SAFETY_ARMED';
  end else Result:=result+rsUnknown;
end;


end.
