'use client'

import { useState, useEffect } from 'react'
import { 
  ChartBarIcon,
  ArrowTrendingUpIcon,
  ArrowTrendingDownIcon,
  UsersIcon,
  CurrencyDollarIcon,
  CalendarIcon,
  MapPinIcon
} from '@heroicons/react/24/outline'
import { Card, StatsCard } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import PageHeader from '@/components/Layout/PageHeader'
import Select from '@/components/UI/Select'
import { formatCurrency } from '@/lib/utils'
import {
  BarChart,
  Bar,
  LineChart,
  Line,
  PieChart,
  Pie,
  Cell,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  Legend,
  ResponsiveContainer
} from 'recharts'

interface AnalyticsSummary {
  total_revenue: number
  revenue_growth: number
  average_order_value: number
  total_customers: number
  new_customers: number
  top_products: any[]
  sales_by_month: any[]
  sales_by_customer: any[]
  sales_by_region: any[]
}

export default function AnalyticsPage() {
  const [period, setPeriod] = useState('last_12_months')
  const [analyticsData, setAnalyticsData] = useState<AnalyticsSummary | null>(null)
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    fetchAnalyticsData()
  }, [period])

  const fetchAnalyticsData = async () => {
    try {
      setLoading(true)
      
      // Fetch real analytics from COBOL endpoint
      const response = await fetch(`http://localhost:8000/api/v1/sales/cobol/analytics?period=${period}`)
      if (response.ok) {
        const data = await response.json()
        setAnalyticsData(data)
      } else {
        console.error('Failed to fetch analytics:', response.status)
      }
    } catch (error) {
      console.error('Failed to fetch analytics:', error)
    } finally {
      setLoading(false)
    }
  }

  const COLORS = ['#3B82F6', '#10B981', '#F59E0B', '#EF4444', '#8B5CF6']

  const periodOptions = [
    { value: 'last_30_days', label: 'Last 30 Days' },
    { value: 'last_quarter', label: 'Last Quarter' },
    { value: 'last_6_months', label: 'Last 6 Months' },
    { value: 'last_12_months', label: 'Last 12 Months' },
    { value: 'year_to_date', label: 'Year to Date' },
    { value: 'last_year', label: 'Last Year' }
  ]

  if (loading) {
    return (
      <div className="min-h-screen bg-gray-50 flex items-center justify-center">
        <div className="text-center">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto"></div>
          <p className="mt-4 text-gray-600">Loading analytics...</p>
        </div>
      </div>
    )
  }

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Sales Analytics"
        description="Sales performance analysis and insights"
        actions={
          <div className="flex items-center space-x-4">
            <Select
              value={period}
              onChange={(e) => setPeriod(e.target.value)}
              options={periodOptions}
              className="w-48"
            />
            <Button variant="outline" size="sm">
              Export Report
            </Button>
          </div>
        }
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'Sales', href: '/sales' },
          { label: 'Analytics' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Summary Stats */}
        {analyticsData && (
          <>
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
              <StatsCard
                title="Total Revenue"
                value={formatCurrency(analyticsData.total_revenue)}
                icon={<CurrencyDollarIcon className="h-6 w-6" />}
                change={{ 
                  value: `${analyticsData.revenue_growth > 0 ? '+' : ''}${analyticsData.revenue_growth}%`, 
                  type: analyticsData.revenue_growth > 0 ? 'increase' : 'decrease' 
                }}
              />
              <StatsCard
                title="Average Order Value"
                value={formatCurrency(analyticsData.average_order_value)}
                icon={<ChartBarIcon className="h-6 w-6" />}
                change={{ 
                  value: 'Per transaction', 
                  type: 'neutral' 
                }}
              />
              <StatsCard
                title="Total Customers"
                value={analyticsData.total_customers.toLocaleString()}
                icon={<UsersIcon className="h-6 w-6" />}
                change={{ 
                  value: `${analyticsData.new_customers} new`, 
                  type: 'increase' 
                }}
              />
              <StatsCard
                title="Growth Trend"
                value={`${analyticsData.revenue_growth}%`}
                icon={analyticsData.revenue_growth > 0 ? 
                  <ArrowTrendingUpIcon className="h-6 w-6" /> : 
                  <ArrowTrendingDownIcon className="h-6 w-6" />
                }
                change={{ 
                  value: 'Year over year', 
                  type: analyticsData.revenue_growth > 0 ? 'increase' : 'decrease' 
                }}
              />
            </div>

            {/* Charts */}
            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-8">
              {/* Sales Trend */}
              <Card>
                <div className="p-6">
                  <h3 className="text-lg font-medium text-gray-900 mb-4">Sales Trend</h3>
                  <ResponsiveContainer width="100%" height={300}>
                    <LineChart data={analyticsData.sales_by_month}>
                      <CartesianGrid strokeDasharray="3 3" />
                      <XAxis dataKey="month" />
                      <YAxis />
                      <Tooltip formatter={(value) => formatCurrency(Number(value))} />
                      <Legend />
                      <Line type="monotone" dataKey="sales" stroke="#3B82F6" name="Sales" />
                    </LineChart>
                  </ResponsiveContainer>
                </div>
              </Card>

              {/* Orders by Month */}
              <Card>
                <div className="p-6">
                  <h3 className="text-lg font-medium text-gray-900 mb-4">Monthly Orders</h3>
                  <ResponsiveContainer width="100%" height={300}>
                    <BarChart data={analyticsData.sales_by_month}>
                      <CartesianGrid strokeDasharray="3 3" />
                      <XAxis dataKey="month" />
                      <YAxis />
                      <Tooltip />
                      <Legend />
                      <Bar dataKey="orders" fill="#10B981" name="Orders" />
                    </BarChart>
                  </ResponsiveContainer>
                </div>
              </Card>
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
              {/* Top Products */}
              <Card>
                <div className="p-6">
                  <h3 className="text-lg font-medium text-gray-900 mb-4">Top Products</h3>
                  <ResponsiveContainer width="100%" height={300}>
                    <PieChart>
                      <Pie
                        data={analyticsData.top_products}
                        cx="50%"
                        cy="50%"
                        labelLine={false}
                        label={({ name, percent }) => `${name} ${(percent * 100).toFixed(0)}%`}
                        outerRadius={80}
                        fill="#8884d8"
                        dataKey="value"
                      >
                        {analyticsData.top_products.map((entry, index) => (
                          <Cell key={`cell-${index}`} fill={COLORS[index % COLORS.length]} />
                        ))}
                      </Pie>
                      <Tooltip formatter={(value) => formatCurrency(Number(value))} />
                    </PieChart>
                  </ResponsiveContainer>
                </div>
              </Card>

              {/* Top Customers */}
              <Card>
                <div className="p-6">
                  <h3 className="text-lg font-medium text-gray-900 mb-4">Top Customers</h3>
                  <div className="space-y-4">
                    {analyticsData.sales_by_customer.map((customer, index) => (
                      <div key={index}>
                        <div className="flex justify-between items-center mb-1">
                          <span className="text-sm font-medium text-gray-900">{customer.name}</span>
                          <span className="text-sm text-gray-500">{formatCurrency(customer.value)}</span>
                        </div>
                        <div className="w-full bg-gray-200 rounded-full h-2">
                          <div
                            className="bg-blue-600 h-2 rounded-full"
                            style={{ width: `${customer.percentage}%` }}
                          />
                        </div>
                      </div>
                    ))}
                  </div>
                </div>
              </Card>

              {/* Sales by Region */}
              <Card>
                <div className="p-6">
                  <h3 className="text-lg font-medium text-gray-900 mb-4">Sales by Region</h3>
                  <ResponsiveContainer width="100%" height={300}>
                    <BarChart data={analyticsData.sales_by_region} layout="vertical">
                      <CartesianGrid strokeDasharray="3 3" />
                      <XAxis type="number" />
                      <YAxis type="category" dataKey="region" />
                      <Tooltip formatter={(value) => formatCurrency(Number(value))} />
                      <Bar dataKey="value" fill="#F59E0B" />
                    </BarChart>
                  </ResponsiveContainer>
                </div>
              </Card>
            </div>
          </>
        )}
      </main>
    </div>
  )
}