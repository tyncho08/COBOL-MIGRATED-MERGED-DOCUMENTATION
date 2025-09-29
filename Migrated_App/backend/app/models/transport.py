"""
Transport Models - Transportation and Logistics Data Structures
Handles carriers, vehicles, routes, and delivery management
"""
from sqlalchemy import Column, String, Integer, Numeric, Text, ForeignKey, Boolean, Date, Time
from sqlalchemy.orm import relationship
from app.core.database import Base


class CarrierRec(Base):
    """Carrier/Transport Company Master Record"""
    __tablename__ = "carrier_rec"
    
    carrier_code = Column(String(10), primary_key=True)
    carrier_name = Column(String(50))
    carrier_address1 = Column(String(50))
    carrier_address2 = Column(String(50))
    carrier_address3 = Column(String(50))
    carrier_postcode = Column(String(10))
    carrier_country = Column(String(3))
    carrier_contact = Column(String(30))
    carrier_phone = Column(String(20))
    carrier_fax = Column(String(20))
    carrier_email = Column(String(50))
    carrier_website = Column(String(50))
    carrier_account_no = Column(String(20))
    carrier_type = Column(String(10))  # ROAD, AIR, SEA, RAIL
    carrier_status = Column(String(10))  # ACTIVE, INACTIVE
    carrier_service_level = Column(String(20))  # STANDARD, EXPRESS, OVERNIGHT
    carrier_insurance_limit = Column(Numeric(15, 2))
    carrier_max_weight = Column(Numeric(10, 2))
    carrier_max_volume = Column(Numeric(10, 2))
    carrier_dangerous_goods = Column(Boolean, default=False)
    carrier_temperature_control = Column(Boolean, default=False)
    carrier_tracking_url = Column(String(100))
    carrier_api_endpoint = Column(String(100))
    carrier_api_key = Column(String(50))
    carrier_cost_per_kg = Column(Numeric(10, 2))
    carrier_cost_per_km = Column(Numeric(10, 2))
    carrier_fuel_surcharge = Column(Numeric(5, 2))
    carrier_payment_terms = Column(Integer)
    carrier_credit_limit = Column(Numeric(15, 2))
    carrier_notes = Column(Text)
    carrier_created_date = Column(Integer)
    carrier_created_by = Column(String(10))
    carrier_updated_date = Column(Integer)
    carrier_updated_by = Column(String(10))


class VehicleRec(Base):
    """Vehicle/Fleet Management Record"""
    __tablename__ = "vehicle_rec"
    
    vehicle_id = Column(String(20), primary_key=True)
    vehicle_registration = Column(String(20))
    vehicle_carrier_code = Column(String(10), ForeignKey('carrier_rec.carrier_code'))
    vehicle_type = Column(String(20))  # TRUCK, VAN, TRAILER, CONTAINER
    vehicle_make = Column(String(30))
    vehicle_model = Column(String(30))
    vehicle_year = Column(Integer)
    vehicle_capacity_weight = Column(Numeric(10, 2))  # kg
    vehicle_capacity_volume = Column(Numeric(10, 2))  # m3
    vehicle_dimensions_length = Column(Numeric(10, 2))  # meters
    vehicle_dimensions_width = Column(Numeric(10, 2))
    vehicle_dimensions_height = Column(Numeric(10, 2))
    vehicle_fuel_type = Column(String(10))  # DIESEL, PETROL, ELECTRIC
    vehicle_fuel_capacity = Column(Numeric(10, 2))
    vehicle_fuel_consumption = Column(Numeric(10, 2))  # liters per 100km
    vehicle_odometer = Column(Integer)
    vehicle_service_due_date = Column(Integer)
    vehicle_service_due_km = Column(Integer)
    vehicle_insurance_expiry = Column(Integer)
    vehicle_tax_expiry = Column(Integer)
    vehicle_mot_expiry = Column(Integer)
    vehicle_status = Column(String(20))  # AVAILABLE, IN_USE, MAINTENANCE, RETIRED
    vehicle_temperature_control = Column(Boolean, default=False)
    vehicle_tail_lift = Column(Boolean, default=False)
    vehicle_gps_tracker_id = Column(String(30))
    vehicle_driver_assigned = Column(String(50))
    vehicle_home_depot = Column(String(10))
    vehicle_current_location = Column(String(50))
    vehicle_notes = Column(Text)
    
    # Relationship
    carrier = relationship("CarrierRec", backref="vehicles")


class RouteRec(Base):
    """Delivery Route Master Record"""
    __tablename__ = "route_rec"
    
    route_code = Column(String(10), primary_key=True)
    route_description = Column(String(50))
    route_carrier_code = Column(String(10), ForeignKey('carrier_rec.carrier_code'))
    route_type = Column(String(20))  # FIXED, DYNAMIC, EXPRESS
    route_frequency = Column(String(20))  # DAILY, WEEKLY, MONTHLY
    route_days = Column(String(7))  # MTWTFSS flags
    route_start_point = Column(String(50))
    route_end_point = Column(String(50))
    route_distance_km = Column(Numeric(10, 2))
    route_duration_hours = Column(Numeric(5, 2))
    route_stops = Column(Integer)
    route_max_weight = Column(Numeric(10, 2))
    route_max_volume = Column(Numeric(10, 2))
    route_max_drops = Column(Integer)
    route_service_level = Column(String(20))
    route_cut_off_time = Column(String(4))  # HHMM
    route_departure_time = Column(String(4))
    route_arrival_time = Column(String(4))
    route_cost = Column(Numeric(10, 2))
    route_zones = Column(Text)  # Comma-separated postal codes/zones
    route_status = Column(String(10))  # ACTIVE, INACTIVE
    route_notes = Column(Text)
    
    # Relationship
    carrier = relationship("CarrierRec", backref="routes")


class DeliveryScheduleRec(Base):
    """Delivery Schedule/Planning Record"""
    __tablename__ = "delivery_schedule_rec"
    
    schedule_id = Column(Integer, primary_key=True, autoincrement=True)
    schedule_date = Column(Integer)
    schedule_route_code = Column(String(10), ForeignKey('route_rec.route_code'))
    schedule_vehicle_id = Column(String(20), ForeignKey('vehicle_rec.vehicle_id'))
    schedule_driver_name = Column(String(50))
    schedule_driver_phone = Column(String(20))
    schedule_departure_planned = Column(String(4))  # HHMM
    schedule_departure_actual = Column(String(4))
    schedule_return_planned = Column(String(4))
    schedule_return_actual = Column(String(4))
    schedule_total_drops = Column(Integer)
    schedule_completed_drops = Column(Integer)
    schedule_failed_drops = Column(Integer)
    schedule_total_weight = Column(Numeric(10, 2))
    schedule_total_volume = Column(Numeric(10, 2))
    schedule_total_distance = Column(Numeric(10, 2))
    schedule_fuel_used = Column(Numeric(10, 2))
    schedule_status = Column(String(20))  # PLANNED, IN_PROGRESS, COMPLETED, CANCELLED
    schedule_notes = Column(Text)
    
    # Relationships
    route = relationship("RouteRec")
    vehicle = relationship("VehicleRec")


class DeliveryStopRec(Base):
    """Individual Delivery Stop/Drop Record"""
    __tablename__ = "delivery_stop_rec"
    
    stop_id = Column(Integer, primary_key=True, autoincrement=True)
    stop_schedule_id = Column(Integer, ForeignKey('delivery_schedule_rec.schedule_id'))
    stop_sequence = Column(Integer)
    stop_despatch_no = Column(String(10))
    stop_customer_code = Column(String(10))
    stop_customer_name = Column(String(50))
    stop_delivery_address1 = Column(String(50))
    stop_delivery_address2 = Column(String(50))
    stop_delivery_address3 = Column(String(50))
    stop_delivery_postcode = Column(String(10))
    stop_contact_name = Column(String(30))
    stop_contact_phone = Column(String(20))
    stop_arrival_planned = Column(String(4))  # HHMM
    stop_arrival_actual = Column(String(4))
    stop_departure_actual = Column(String(4))
    stop_packages = Column(Integer)
    stop_weight = Column(Numeric(10, 2))
    stop_signature = Column(String(50))
    stop_status = Column(String(20))  # PENDING, DELIVERED, FAILED, PARTIAL
    stop_failure_reason = Column(String(50))
    stop_notes = Column(Text)
    stop_photo_url = Column(String(100))
    stop_gps_latitude = Column(Numeric(10, 6))
    stop_gps_longitude = Column(Numeric(10, 6))
    
    # Relationship
    schedule = relationship("DeliveryScheduleRec", backref="stops")


class TransportCostRec(Base):
    """Transport Cost/Rating Record"""
    __tablename__ = "transport_cost_rec"
    
    cost_id = Column(Integer, primary_key=True, autoincrement=True)
    cost_carrier_code = Column(String(10), ForeignKey('carrier_rec.carrier_code'))
    cost_zone_from = Column(String(10))
    cost_zone_to = Column(String(10))
    cost_service_level = Column(String(20))
    cost_weight_from = Column(Numeric(10, 2))
    cost_weight_to = Column(Numeric(10, 2))
    cost_base_charge = Column(Numeric(10, 2))
    cost_per_kg = Column(Numeric(10, 2))
    cost_per_km = Column(Numeric(10, 2))
    cost_fuel_surcharge_pct = Column(Numeric(5, 2))
    cost_min_charge = Column(Numeric(10, 2))
    cost_max_charge = Column(Numeric(10, 2))
    cost_effective_date = Column(Integer)
    cost_expiry_date = Column(Integer)
    cost_currency = Column(String(3))
    cost_status = Column(String(10))  # ACTIVE, INACTIVE
    
    # Relationship
    carrier = relationship("CarrierRec")