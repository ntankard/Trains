with Raildefs;
package command_Dec is

   type Speed_change is( speed_up, slow_down);
   type Path_change is (Left, Right, Center); --not the same as turned or stragith!!

   type User_input_type is (Speed, Polarity, Path);

   type User_input_record(input_type: User_input_type := Speed) is
      record
            case input_type is
               when Speed=>
               		Speed_increase: Speed_change ;
                  when Polarity =>
                     Polarity_update: raildefs.Polarity_Type;
                  when Path =>
                  Path_preference: Path_change;
            end case;
         end record;


 type Event_Type is (user_input, hall_event, HeartPulse);

   type Command_Type(Eve : Event_Type := user_input) is
      record
         case Eve is
            when user_input =>
               Input : User_input_record;
            when hall_event =>
               Sensor: Raildefs.Sensor_Idx;
               State : Raildefs.Sensor_Bit;
            when HeartPulse =>
               null;
         end case;
      end record;
end command_Dec;
