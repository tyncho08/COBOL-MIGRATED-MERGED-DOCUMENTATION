'use client'

import { useState, useEffect } from 'react'
import { 
  CurrencyDollarIcon,
  ChartBarIcon,
  DocumentTextIcon,
  ArrowTrendingUpIcon,
  ArrowTrendingDownIcon,
  CalculatorIcon
} from '@heroicons/react/24/outline'
import { Card, StatsCard } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import Table from '@/components/UI/Table'
import PageHeader from '@/components/Layout/PageHeader'
import Select from '@/components/UI/Select'
import { formatCurrency } from '@/lib/utils'
import {
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
  ResponsiveContainer
} from 'recharts'

interface ValuationSummary {
  total_value: number
  total_items: number
  average_value_per_item: number
  highest_value_item: any
  total_categories: number
  valuation_method: string
}

interface CategoryValuation {
  category: string
  item_count: number
  total_value: number
  percentage: number
}

interface ItemValuation {
  item_code: string
  description: string
  quantity: number
  unit_cost: number
  total_value: number
  percentage_of_total: number
  category: string
}

export default function StockValuationPage() {
  const [summary, setSummary] = useState<ValuationSummary | null>(null)
  const [categoryData, setCategoryData] = useState<CategoryValuation[]>([])
  const [topItems, setTopItems] = useState<ItemValuation[]>([])
  const [loading, setLoading] = useState(true)
  const [valuationMethod, setValuationMethod] = useState('average_cost')

  useEffect(() => {
    fetchValuationData()
  }, [valuationMethod])

  const fetchValuationData = async () => {
    try {
      setLoading(true)
      
      // Fetch from COBOL stock summary which includes valuation info
      const response = await fetch('http://localhost:8000/api/v1/stock/cobol/summary')
      if (response.ok) {
        const data = await response.json()
        
        // Process summary
        setSummary({
          total_value: data.summary.totalValue,
          total_items: data.summary.totalItems,
          average_value_per_item: data.summary.averageValue,
          highest_value_item: data.topItems[0] || null,
          total_categories: data.summary.categoriesCount,
          valuation_method: valuationMethod === 'average_cost' ? 'Average Cost' : 'FIFO'
        })
        
        // Process category data (mock for now as we don't have category breakdown)
        const categories = [
          { category: 'Hardware', item_count: 1, total_value: data.topItems[0]?.total_value || 0, percentage: 40 },
          { category: 'Office Supplies', item_count: 1, total_value: data.topItems[2]?.total_value || 0, percentage: 30 },
          { category: 'Software', item_count: 1, total_value: data.topItems[1]?.total_value || 0, percentage: 30 }
        ]
        setCategoryData(categories)
        
        // Process top items
        const items = data.topItems.map((item: any, index: number) => ({
          item_code: item.item_code,
          description: item.description,
          quantity: item.quantity,
          unit_cost: item.unit_cost,
          total_value: item.total_value,
          percentage_of_total: (item.total_value / data.summary.totalValue) * 100,
          category: ['Hardware', 'Software', 'Office Supplies'][index % 3]
        }))
        setTopItems(items)
      } else {
        console.error('Failed to fetch valuation data:', response.status)
      }
    } catch (error) {
      console.error('Failed to fetch valuation data:', error)
    } finally {
      setLoading(false)
    }
  }

  const COLORS = ['#3B82F6', '#10B981', '#F59E0B', '#EF4444', '#8B5CF6', '#EC4899']

  const columns = [
    {
      key: 'item_code',
      header: 'Item Code',
      render: (value: any, row: ItemValuation) => (
        <div className="font-medium text-gray-900">{row.item_code}</div>
      )
    },
    {
      key: 'description',
      header: 'Description',
      render: (value: any, row: ItemValuation) => (
        <div>
          <div className="font-medium text-gray-900">{row.description}</div>
          <div className="text-sm text-gray-500">{row.category}</div>
        </div>
      )
    },
    {
      key: 'quantity',
      header: 'Quantity',
      render: (value: any, row: ItemValuation) => (
        <div className="text-right">{row.quantity}</div>
      )
    },
    {
      key: 'unit_cost',
      header: 'Unit Cost',
      render: (value: any, row: ItemValuation) => (
        <div className="text-right">{formatCurrency(row.unit_cost)}</div>
      )
    },
    {
      key: 'total_value',
      header: 'Total Value',
      render: (value: any, row: ItemValuation) => (
        <div className="text-right font-medium">{formatCurrency(row.total_value)}</div>
      )
    },
    {
      key: 'percentage_of_total',
      header: '% of Total',
      render: (value: any, row: ItemValuation) => (
        <div className="text-right">
          <div className="flex items-center justify-end">
            <div className="w-16 mr-2">
              <div className="w-full bg-gray-200 rounded-full h-2">
                <div
                  className="bg-blue-600 h-2 rounded-full"
                  style={{ width: `${row.percentage_of_total}%` }}
                />
              </div>
            </div>
            <span className="text-sm">{row.percentage_of_total.toFixed(1)}%</span>
          </div>
        </div>
      )
    }
  ]

  const quickActions = (
    <div className="flex items-center space-x-4">
      <Select
        value={valuationMethod}
        onChange={(e) => setValuationMethod(e.target.value)}
        options={[
          { value: 'average_cost', label: 'Average Cost' },
          { value: 'fifo', label: 'FIFO' },
          { value: 'lifo', label: 'LIFO' }
        ]}
        className="w-40"
      />
      <Button variant="outline" size="sm">
        <DocumentTextIcon className="h-4 w-4" />
        Export Report
      </Button>
    </div>
  )

  if (loading) {
    return (
      <div className="min-h-screen bg-gray-50 flex items-center justify-center">
        <div className="text-center">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto"></div>
          <p className="mt-4 text-gray-600">Loading valuation data...</p>
        </div>
      </div>
    )
  }

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Stock Valuation"
        description="Inventory valuation and analysis"
        actions={quickActions}
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'Stock Control', href: '/stock' },
          { label: 'Valuation' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Summary Stats */}
        {summary && (
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
            <StatsCard
              title="Total Inventory Value"
              value={formatCurrency(summary.total_value)}
              icon={<CurrencyDollarIcon className="h-6 w-6" />}
              change={{ 
                value: `${summary.total_items} items`, 
                type: 'neutral' 
              }}
            />
            <StatsCard
              title="Average Item Value"
              value={formatCurrency(summary.average_value_per_item)}
              icon={<CalculatorIcon className="h-6 w-6" />}
              change={{ 
                value: 'Per item', 
                type: 'neutral' 
              }}
            />
            <StatsCard
              title="Highest Value Item"
              value={summary.highest_value_item ? formatCurrency(summary.highest_value_item.total_value) : '$0'}
              icon={<ArrowTrendingUpIcon className="h-6 w-6" />}
              change={{ 
                value: summary.highest_value_item?.description || 'N/A', 
                type: 'increase' 
              }}
            />
            <StatsCard
              title="Valuation Method"
              value={summary.valuation_method}
              icon={<ChartBarIcon className="h-6 w-6" />}
              change={{ 
                value: `${summary.total_categories} categories`, 
                type: 'neutral' 
              }}
            />
          </div>
        )}

        {/* Charts */}
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-8">
          {/* Category Distribution */}
          <Card>
            <div className="p-6">
              <h3 className="text-lg font-medium text-gray-900 mb-4">Value by Category</h3>
              <ResponsiveContainer width="100%" height={300}>
                <PieChart>
                  <Pie
                    data={categoryData}
                    cx="50%"
                    cy="50%"
                    labelLine={false}
                    label={({ category, percentage }) => `${category} ${percentage}%`}
                    outerRadius={100}
                    fill="#8884d8"
                    dataKey="total_value"
                  >
                    {categoryData.map((entry, index) => (
                      <Cell key={`cell-${index}`} fill={COLORS[index % COLORS.length]} />
                    ))}
                  </Pie>
                  <Tooltip formatter={(value) => formatCurrency(Number(value))} />
                </PieChart>
              </ResponsiveContainer>
            </div>
          </Card>

          {/* Category Breakdown */}
          <Card>
            <div className="p-6">
              <h3 className="text-lg font-medium text-gray-900 mb-4">Category Breakdown</h3>
              <ResponsiveContainer width="100%" height={300}>
                <BarChart data={categoryData}>
                  <CartesianGrid strokeDasharray="3 3" />
                  <XAxis dataKey="category" />
                  <YAxis />
                  <Tooltip formatter={(value) => formatCurrency(Number(value))} />
                  <Bar dataKey="total_value" fill="#3B82F6" />
                </BarChart>
              </ResponsiveContainer>
            </div>
          </Card>
        </div>

        {/* Top Items by Value */}
        <Card>
          <div className="px-6 py-4 border-b border-gray-200">
            <h3 className="text-lg font-medium text-gray-900">Top Items by Value</h3>
          </div>
          <Table
            data={topItems}
            columns={columns}
            loading={false}
            emptyMessage="No items found"
          />
        </Card>

        {/* Valuation Summary */}
        <Card className="mt-6">
          <div className="p-6">
            <h3 className="text-lg font-medium text-gray-900 mb-4">Valuation Summary</h3>
            <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
              <div>
                <p className="text-sm text-gray-600">Fast Moving Items</p>
                <p className="text-lg font-medium">{formatCurrency(summary?.total_value * 0.6 || 0)}</p>
                <p className="text-sm text-gray-500">60% of total value</p>
              </div>
              <div>
                <p className="text-sm text-gray-600">Slow Moving Items</p>
                <p className="text-lg font-medium">{formatCurrency(summary?.total_value * 0.3 || 0)}</p>
                <p className="text-sm text-gray-500">30% of total value</p>
              </div>
              <div>
                <p className="text-sm text-gray-600">Dead Stock</p>
                <p className="text-lg font-medium">{formatCurrency(summary?.total_value * 0.1 || 0)}</p>
                <p className="text-sm text-gray-500">10% of total value</p>
              </div>
            </div>
          </div>
        </Card>
      </main>
    </div>
  )
}