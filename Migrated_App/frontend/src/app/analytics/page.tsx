'use client'

import { useState, useEffect } from 'react'
import {
  ChartBarIcon,
  ArrowTrendingUpIcon,
  ArrowTrendingDownIcon,
  CurrencyDollarIcon,
  UserGroupIcon,
  ShoppingCartIcon,
  ClockIcon,
  CalendarIcon,
  ArrowPathIcon
} from '@heroicons/react/24/outline'
import { Card, StatsCard } from '@/components/UI/Card'
import PageHeader from '@/components/Layout/PageHeader'
import {
  LineChart,
  Line,
  BarChart,
  Bar,
  PieChart,
  Pie,
  Cell,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  Legend,
  ResponsiveContainer,
  AreaChart,
  Area
} from 'recharts'

interface AnalyticsData {
  revenue: {
    daily: any[]
    monthly: any[]
    yearly: any[]
  }
  customers: {
    growth: any[]
    segments: any[]
    topCustomers: any[]
  }
  inventory: {
    turnover: any[]
    categories: any[]
    movements: any[]
  }
  financial: {
    cashFlow: any[]
    profitMargin: any[]
    expenses: any[]
  }
}

export default function AnalyticsPage() {
  const [loading, setLoading] = useState(true)
  const [timeRange, setTimeRange] = useState('30days')
  const [analyticsData, setAnalyticsData] = useState<AnalyticsData | null>(null)
  const [refreshing, setRefreshing] = useState(false)

  useEffect(() => {
    fetchAnalyticsData()
  }, [timeRange])

  const fetchAnalyticsData = async () => {
    setLoading(true)
    try {
      const response = await fetch(`http://localhost:8000/api/v1/analytics/dashboard?range=${timeRange}`)
      if (response.ok) {
        const data = await response.json()
        setAnalyticsData(data)
      } else {
        // Use mock data
        setAnalyticsData(getMockAnalyticsData())
      }
    } catch (error) {
      console.error('Failed to fetch analytics:', error)
      setAnalyticsData(getMockAnalyticsData())
    } finally {
      setLoading(false)
    }
  }

  const handleRefresh = async () => {
    setRefreshing(true)
    await fetchAnalyticsData()
    setTimeout(() => setRefreshing(false), 1000)
  }

  const getMockAnalyticsData = (): AnalyticsData => {
    const generateDailyData = (days: number) => {
      return Array.from({ length: days }, (_, i) => {
        const date = new Date()
        date.setDate(date.getDate() - (days - i - 1))
        return {
          date: date.toISOString().split('T')[0],
          revenue: Math.floor(Math.random() * 10000) + 5000,
          orders: Math.floor(Math.random() * 50) + 20,
          customers: Math.floor(Math.random() * 30) + 10
        }
      })
    }

    return {
      revenue: {
        daily: generateDailyData(30),
        monthly: [
          { month: 'Jan', revenue: 125000, target: 120000 },
          { month: 'Feb', revenue: 132000, target: 130000 },
          { month: 'Mar', revenue: 145000, target: 140000 },
          { month: 'Apr', revenue: 138000, target: 145000 },
          { month: 'May', revenue: 155000, target: 150000 },
          { month: 'Jun', revenue: 162000, target: 160000 }
        ],
        yearly: [
          { year: '2022', revenue: 1450000 },
          { year: '2023', revenue: 1680000 },
          { year: '2024', revenue: 920000 }
        ]
      },
      customers: {
        growth: [
          { month: 'Jan', newCustomers: 45, totalCustomers: 1250 },
          { month: 'Feb', newCustomers: 52, totalCustomers: 1302 },
          { month: 'Mar', newCustomers: 38, totalCustomers: 1340 },
          { month: 'Apr', newCustomers: 61, totalCustomers: 1401 },
          { month: 'May', newCustomers: 55, totalCustomers: 1456 },
          { month: 'Jun', newCustomers: 48, totalCustomers: 1504 }
        ],
        segments: [
          { name: 'Enterprise', value: 35, count: 126 },
          { name: 'Mid-Market', value: 40, count: 144 },
          { name: 'Small Business', value: 25, count: 90 }
        ],
        topCustomers: [
          { name: 'ABC Corporation', revenue: 125000, orders: 156 },
          { name: 'XYZ Limited', revenue: 98000, orders: 134 },
          { name: 'Tech Solutions Inc', revenue: 87000, orders: 112 },
          { name: 'Global Trading Co', revenue: 76000, orders: 98 },
          { name: 'Prime Industries', revenue: 65000, orders: 87 }
        ]
      },
      inventory: {
        turnover: [
          { month: 'Jan', turnoverRate: 4.2, daysOnHand: 28 },
          { month: 'Feb', turnoverRate: 4.5, daysOnHand: 26 },
          { month: 'Mar', turnoverRate: 4.1, daysOnHand: 29 },
          { month: 'Apr', turnoverRate: 4.8, daysOnHand: 24 },
          { month: 'May', turnoverRate: 5.1, daysOnHand: 22 },
          { month: 'Jun', turnoverRate: 4.9, daysOnHand: 23 }
        ],
        categories: [
          { category: 'Electronics', value: 35, items: 245 },
          { category: 'Accessories', value: 25, items: 189 },
          { category: 'Components', value: 20, items: 156 },
          { category: 'Consumables', value: 20, items: 167 }
        ],
        movements: generateDailyData(7).map(d => ({
          date: d.date,
          receipts: Math.floor(Math.random() * 50) + 20,
          issues: Math.floor(Math.random() * 40) + 15,
          adjustments: Math.floor(Math.random() * 10) - 5
        }))
      },
      financial: {
        cashFlow: [
          { month: 'Jan', inflow: 145000, outflow: 112000, net: 33000 },
          { month: 'Feb', inflow: 152000, outflow: 118000, net: 34000 },
          { month: 'Mar', inflow: 168000, outflow: 125000, net: 43000 },
          { month: 'Apr', inflow: 155000, outflow: 132000, net: 23000 },
          { month: 'May', inflow: 172000, outflow: 128000, net: 44000 },
          { month: 'Jun', inflow: 180000, outflow: 135000, net: 45000 }
        ],
        profitMargin: [
          { month: 'Jan', gross: 42, net: 18, operating: 25 },
          { month: 'Feb', gross: 43, net: 19, operating: 26 },
          { month: 'Mar', gross: 41, net: 17, operating: 24 },
          { month: 'Apr', gross: 44, net: 20, operating: 27 },
          { month: 'May', gross: 45, net: 21, operating: 28 },
          { month: 'Jun', gross: 43, net: 19, operating: 26 }
        ],
        expenses: [
          { category: 'Payroll', amount: 45000, percentage: 35 },
          { category: 'Rent & Utilities', amount: 18000, percentage: 14 },
          { category: 'Marketing', amount: 12000, percentage: 9 },
          { category: 'Operations', amount: 25000, percentage: 20 },
          { category: 'Other', amount: 28000, percentage: 22 }
        ]
      }
    }
  }

  const COLORS = ['#4F46E5', '#10B981', '#F59E0B', '#EF4444', '#8B5CF6', '#EC4899']

  const formatCurrency = (value: number) => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
      minimumFractionDigits: 0,
      maximumFractionDigits: 0
    }).format(value)
  }

  const data = analyticsData || getMockAnalyticsData()

  // Calculate KPIs
  const currentMonthRevenue = data.revenue.monthly[data.revenue.monthly.length - 1]?.revenue || 0
  const previousMonthRevenue = data.revenue.monthly[data.revenue.monthly.length - 2]?.revenue || 0
  const revenueGrowth = previousMonthRevenue ? ((currentMonthRevenue - previousMonthRevenue) / previousMonthRevenue * 100) : 0

  const totalCustomers = data.customers.growth[data.customers.growth.length - 1]?.totalCustomers || 0
  const newCustomersThisMonth = data.customers.growth[data.customers.growth.length - 1]?.newCustomers || 0
  
  const avgTurnoverRate = data.inventory.turnover.reduce((sum, item) => sum + item.turnoverRate, 0) / data.inventory.turnover.length
  const currentCashFlow = data.financial.cashFlow[data.financial.cashFlow.length - 1]?.net || 0

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Analytics Dashboard"
        description="Business intelligence and performance metrics"
        actions={
          <div className="flex items-center gap-3">
            <select
              className="block rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500 sm:text-sm"
              value={timeRange}
              onChange={(e) => setTimeRange(e.target.value)}
            >
              <option value="7days">Last 7 Days</option>
              <option value="30days">Last 30 Days</option>
              <option value="90days">Last 90 Days</option>
              <option value="1year">Last Year</option>
            </select>
            <button
              onClick={handleRefresh}
              className={`p-2 text-gray-400 hover:text-gray-500 ${refreshing ? 'animate-spin' : ''}`}
              disabled={refreshing}
            >
              <ArrowPathIcon className="h-5 w-5" />
            </button>
          </div>
        }
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'Analytics' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {loading ? (
          <div className="flex items-center justify-center h-96">
            <div className="text-center">
              <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-indigo-600 mx-auto"></div>
              <p className="mt-4 text-gray-500">Loading analytics data...</p>
            </div>
          </div>
        ) : (
          <>
            {/* KPI Cards */}
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
              <StatsCard
                title="Revenue"
                value={formatCurrency(currentMonthRevenue)}
                icon={<CurrencyDollarIcon className="h-6 w-6" />}
                change={{
                  value: `${revenueGrowth > 0 ? '+' : ''}${revenueGrowth.toFixed(1)}%`,
                  type: revenueGrowth > 0 ? 'increase' : 'decrease'
                }}
              />
              <StatsCard
                title="Total Customers"
                value={totalCustomers.toString()}
                icon={<UserGroupIcon className="h-6 w-6" />}
                change={{
                  value: `+${newCustomersThisMonth} this month`,
                  type: 'increase'
                }}
              />
              <StatsCard
                title="Inventory Turnover"
                value={avgTurnoverRate.toFixed(1) + 'x'}
                icon={<ShoppingCartIcon className="h-6 w-6" />}
                change={{
                  value: 'Avg days on hand: ' + Math.round(365 / avgTurnoverRate),
                  type: 'neutral'
                }}
              />
              <StatsCard
                title="Net Cash Flow"
                value={formatCurrency(currentCashFlow)}
                icon={<ArrowTrendingUpIcon className="h-6 w-6" />}
                change={{
                  value: 'This month',
                  type: currentCashFlow > 0 ? 'increase' : 'decrease'
                }}
              />
            </div>

            {/* Charts Grid */}
            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-8">
              {/* Revenue Trend */}
              <Card>
                <div className="p-6">
                  <h3 className="text-lg font-medium text-gray-900 mb-4">Revenue Trend</h3>
                  <ResponsiveContainer width="100%" height={300}>
                    <AreaChart data={data.revenue.monthly}>
                      <CartesianGrid strokeDasharray="3 3" />
                      <XAxis dataKey="month" />
                      <YAxis tickFormatter={(value) => `$${value / 1000}k`} />
                      <Tooltip formatter={(value: any) => formatCurrency(value)} />
                      <Legend />
                      <Area
                        type="monotone"
                        dataKey="revenue"
                        stroke="#4F46E5"
                        fill="#4F46E5"
                        fillOpacity={0.1}
                        strokeWidth={2}
                      />
                      <Area
                        type="monotone"
                        dataKey="target"
                        stroke="#10B981"
                        fill="#10B981"
                        fillOpacity={0.1}
                        strokeWidth={2}
                      />
                    </AreaChart>
                  </ResponsiveContainer>
                </div>
              </Card>

              {/* Customer Growth */}
              <Card>
                <div className="p-6">
                  <h3 className="text-lg font-medium text-gray-900 mb-4">Customer Growth</h3>
                  <ResponsiveContainer width="100%" height={300}>
                    <LineChart data={data.customers.growth}>
                      <CartesianGrid strokeDasharray="3 3" />
                      <XAxis dataKey="month" />
                      <YAxis />
                      <Tooltip />
                      <Legend />
                      <Line
                        type="monotone"
                        dataKey="totalCustomers"
                        stroke="#4F46E5"
                        strokeWidth={2}
                        name="Total Customers"
                      />
                      <Bar dataKey="newCustomers" fill="#10B981" name="New Customers" />
                    </LineChart>
                  </ResponsiveContainer>
                </div>
              </Card>
            </div>

            {/* Second Row Charts */}
            <div className="grid grid-cols-1 lg:grid-cols-3 gap-6 mb-8">
              {/* Customer Segments */}
              <Card>
                <div className="p-6">
                  <h3 className="text-lg font-medium text-gray-900 mb-4">Customer Segments</h3>
                  <ResponsiveContainer width="100%" height={250}>
                    <PieChart>
                      <Pie
                        data={data.customers.segments}
                        cx="50%"
                        cy="50%"
                        labelLine={false}
                        label={({ name, value }) => `${name}: ${value}%`}
                        outerRadius={80}
                        fill="#8884d8"
                        dataKey="value"
                      >
                        {data.customers.segments.map((entry, index) => (
                          <Cell key={`cell-${index}`} fill={COLORS[index % COLORS.length]} />
                        ))}
                      </Pie>
                      <Tooltip />
                    </PieChart>
                  </ResponsiveContainer>
                  <div className="mt-4 space-y-2">
                    {data.customers.segments.map((segment, index) => (
                      <div key={segment.name} className="flex items-center justify-between text-sm">
                        <div className="flex items-center">
                          <div 
                            className="w-3 h-3 rounded-full mr-2" 
                            style={{ backgroundColor: COLORS[index % COLORS.length] }}
                          />
                          <span className="text-gray-600">{segment.name}</span>
                        </div>
                        <span className="font-medium">{segment.count} customers</span>
                      </div>
                    ))}
                  </div>
                </div>
              </Card>

              {/* Inventory Categories */}
              <Card>
                <div className="p-6">
                  <h3 className="text-lg font-medium text-gray-900 mb-4">Inventory Distribution</h3>
                  <ResponsiveContainer width="100%" height={250}>
                    <PieChart>
                      <Pie
                        data={data.inventory.categories}
                        cx="50%"
                        cy="50%"
                        innerRadius={40}
                        outerRadius={80}
                        fill="#8884d8"
                        paddingAngle={5}
                        dataKey="value"
                      >
                        {data.inventory.categories.map((entry, index) => (
                          <Cell key={`cell-${index}`} fill={COLORS[index % COLORS.length]} />
                        ))}
                      </Pie>
                      <Tooltip />
                    </PieChart>
                  </ResponsiveContainer>
                  <div className="mt-4 space-y-2">
                    {data.inventory.categories.map((category, index) => (
                      <div key={category.category} className="flex items-center justify-between text-sm">
                        <div className="flex items-center">
                          <div 
                            className="w-3 h-3 rounded-full mr-2" 
                            style={{ backgroundColor: COLORS[index % COLORS.length] }}
                          />
                          <span className="text-gray-600">{category.category}</span>
                        </div>
                        <span className="font-medium">{category.items} items</span>
                      </div>
                    ))}
                  </div>
                </div>
              </Card>

              {/* Expense Breakdown */}
              <Card>
                <div className="p-6">
                  <h3 className="text-lg font-medium text-gray-900 mb-4">Expense Breakdown</h3>
                  <ResponsiveContainer width="100%" height={250}>
                    <BarChart data={data.financial.expenses} layout="horizontal">
                      <CartesianGrid strokeDasharray="3 3" />
                      <XAxis type="number" tickFormatter={(value) => `$${value / 1000}k`} />
                      <YAxis dataKey="category" type="category" width={80} />
                      <Tooltip formatter={(value: any) => formatCurrency(value)} />
                      <Bar dataKey="amount" fill="#EF4444" />
                    </BarChart>
                  </ResponsiveContainer>
                </div>
              </Card>
            </div>

            {/* Cash Flow & Profit Margins */}
            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-8">
              {/* Cash Flow */}
              <Card>
                <div className="p-6">
                  <h3 className="text-lg font-medium text-gray-900 mb-4">Cash Flow Analysis</h3>
                  <ResponsiveContainer width="100%" height={300}>
                    <BarChart data={data.financial.cashFlow}>
                      <CartesianGrid strokeDasharray="3 3" />
                      <XAxis dataKey="month" />
                      <YAxis tickFormatter={(value) => `$${value / 1000}k`} />
                      <Tooltip formatter={(value: any) => formatCurrency(value)} />
                      <Legend />
                      <Bar dataKey="inflow" fill="#10B981" name="Cash In" />
                      <Bar dataKey="outflow" fill="#EF4444" name="Cash Out" />
                      <Line type="monotone" dataKey="net" stroke="#4F46E5" strokeWidth={2} name="Net" />
                    </BarChart>
                  </ResponsiveContainer>
                </div>
              </Card>

              {/* Profit Margins */}
              <Card>
                <div className="p-6">
                  <h3 className="text-lg font-medium text-gray-900 mb-4">Profit Margins Trend</h3>
                  <ResponsiveContainer width="100%" height={300}>
                    <LineChart data={data.financial.profitMargin}>
                      <CartesianGrid strokeDasharray="3 3" />
                      <XAxis dataKey="month" />
                      <YAxis tickFormatter={(value) => `${value}%`} />
                      <Tooltip formatter={(value: any) => `${value}%`} />
                      <Legend />
                      <Line
                        type="monotone"
                        dataKey="gross"
                        stroke="#10B981"
                        strokeWidth={2}
                        name="Gross Margin"
                      />
                      <Line
                        type="monotone"
                        dataKey="operating"
                        stroke="#F59E0B"
                        strokeWidth={2}
                        name="Operating Margin"
                      />
                      <Line
                        type="monotone"
                        dataKey="net"
                        stroke="#4F46E5"
                        strokeWidth={2}
                        name="Net Margin"
                      />
                    </LineChart>
                  </ResponsiveContainer>
                </div>
              </Card>
            </div>

            {/* Top Customers Table */}
            <Card>
              <div className="px-6 py-4 border-b border-gray-200">
                <h3 className="text-lg font-medium text-gray-900">Top Customers</h3>
              </div>
              <div className="overflow-x-auto">
                <table className="min-w-full divide-y divide-gray-200">
                  <thead className="bg-gray-50">
                    <tr>
                      <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                        Customer
                      </th>
                      <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                        Revenue
                      </th>
                      <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                        Orders
                      </th>
                      <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                        Avg Order Value
                      </th>
                    </tr>
                  </thead>
                  <tbody className="bg-white divide-y divide-gray-200">
                    {data.customers.topCustomers.map((customer, index) => (
                      <tr key={customer.name} className="hover:bg-gray-50">
                        <td className="px-6 py-4 whitespace-nowrap">
                          <div className="flex items-center">
                            <div className="flex-shrink-0">
                              <div className="h-10 w-10 rounded-full bg-indigo-100 flex items-center justify-center">
                                <span className="text-indigo-600 font-medium text-sm">
                                  {index + 1}
                                </span>
                              </div>
                            </div>
                            <div className="ml-4">
                              <div className="text-sm font-medium text-gray-900">{customer.name}</div>
                            </div>
                          </div>
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap">
                          <div className="text-sm text-gray-900">{formatCurrency(customer.revenue)}</div>
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap">
                          <div className="text-sm text-gray-900">{customer.orders}</div>
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap">
                          <div className="text-sm text-gray-900">
                            {formatCurrency(customer.revenue / customer.orders)}
                          </div>
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </Card>

            {/* Daily Activity Heatmap */}
            <Card className="mt-6">
              <div className="p-6">
                <h3 className="text-lg font-medium text-gray-900 mb-4">Daily Revenue Pattern</h3>
                <ResponsiveContainer width="100%" height={200}>
                  <BarChart data={data.revenue.daily.slice(-14)}>
                    <CartesianGrid strokeDasharray="3 3" />
                    <XAxis 
                      dataKey="date" 
                      tickFormatter={(date) => new Date(date).toLocaleDateString('en', { day: 'numeric', month: 'short' })}
                    />
                    <YAxis tickFormatter={(value) => `$${value / 1000}k`} />
                    <Tooltip 
                      formatter={(value: any) => formatCurrency(value)}
                      labelFormatter={(date) => new Date(date).toLocaleDateString()}
                    />
                    <Bar dataKey="revenue" fill="#4F46E5" />
                  </BarChart>
                </ResponsiveContainer>
              </div>
            </Card>
          </>
        )}
      </main>
    </div>
  )
}